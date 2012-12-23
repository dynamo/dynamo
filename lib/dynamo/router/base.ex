defexception Dynamo.Router.InvalidHookError, kind: nil, hook: nil, actual: nil do
  def message(exception) do
    "expected #{exception.kind} hook #{inspect exception.hook} to return " <>
      "a HTTP connection, but got #{inspect exception.actual}"
  end
end

defmodule Dynamo.Router.Base do
  @moduledoc """
  This module contains the basic structure for a `Dynamo.Router`.

  It provides a set of macros to generate routes as well as
  preparing and finalizing requests. For example:

      defmodule HomeRouter do
        use Dynamo.Router

        prepare do
          conn.assign :layout, "hello.html"
        end

        get "/hello" do
          conn.resp 200, "world"
        end

        forward "/posts", to: PostsRouter
      end

  ## Routes

      get "/hello" do
        conn.resp 200, "world"
      end

  In the example above, a request will only match if it is
  a `GET` request and the route "/hello". The supported
  verbs are `get`, `post`, `put` and `delete`.

  A route can also specify parameters which will then be
  available in the function body:

      get "/hello/:name" do
        conn.resp 200, "hello \#{name}"
      end

  Finally, a general `match` function is also supported:

      match "/hello" do
        conn.resp 200, "world"
      end

  A `match` will match any route regardless of the HTTP verb.
  Check `match/3` for more information on how routes compilation
  work and a list of supported options.

  ## Forwarding

  A Dynamo router can also forward a specific route to any other
  router, allowing a developer to scope its application instead of
  having a big monolitic routes handler:

      defmodule ApplicationRouter do
        use Dynamo.Router
        forward "/home", to: HomeRouter
      end

  Now any request starting with "/home" in `ApplicationRouter` router
  will be forwarded to `HomeRouter`, but without the "/home" prefix.

  Therefore a request to "/home/hello" is seen by the `HomeRouter`
  simply as "/hello", matching the route we defined at the beginning
  of this section.

  Although in the example above we forwarded to another Dynamo router,
  we can forward to any module, as long as it exports the function
  `service/1`. This function receives the connection as argument and
  must return a (possibly updated) connection.

  ## Hooks

  This module is provides both `prepare/1` and `finalize/1` hooks to
  routers. Such hooks are useful to check conditions, fetch aspects
  or updating the connection:

      prepare do
        conn.assign :layout, "hello.html"
      end

      prepare :check_authentication

      defp check_authentication do
        unless conn.session[:user_id] do
          redirect! conn, to: "/"
        end
      end

  Such hooks can be defined in the following formats:

  * `:function_name` - the atom denotes the function name to be invoked
    in the current module;

  * `module_or_tuple` - a module or a tuple where `prepare/1` or `finalize/1`
     will be invoked passing the connection as argument;

  * `[do: block]` - a chunk of code to be executed as hook. The block
    has access to the connection as `conn`.

  Hooks receive the connection as argument and must return the updated
  connection (if any change happens).
  """

  @doc false
  defmacro __using__(_) do
    [ quote do
        Enum.each [:__prepare_hooks, :__finalize_hooks],
          Module.register_attribute(__MODULE__, &1, accumulate: true, persist: false)
        @before_compile unquote(__MODULE__)
        import unquote(__MODULE__)
      end,

      quote location: :keep do
        @doc false
        def service(conn) do
          dispatch(conn.method, conn.path_info_segments, conn)
        end

        @doc false
        def not_found(conn) do
          conn.resp(404, "Not found")
        end

        defoverridable [not_found: 1, service: 1]
      end ]
  end

  @doc false
  defmacro __before_compile__(module) do
    prepare  = Module.get_attribute(module, :__prepare_hooks)
    finalize = Module.get_attribute(module, :__finalize_hooks)

    prepare = Enum.reduce prepare, quote(do: conn), fn(hook, acc) ->
      compile_hook(hook, acc, :prepare, function(:compile_prepare, 3))
    end

    finalize = Enum.reduce finalize, quote(do: conn), fn(hook, acc) ->
      compile_hook(hook, acc, :finalize, function(:compile_finalize, 3))
    end

    quote location: :keep do
      @doc false
      def run_prepare_hooks(conn),  do: unquote(prepare)

      @doc false
      def run_finalize_hooks(conn), do: unquote(finalize)

      @doc false
      def dispatch(_, _, conn) do
        not_found(conn)
      end
    end
  end

  ## Match

  @doc """
  Main API to define routes. It accepts an expression representing
  the path and many options allowing the match to be configured.

  ## Examples

      match "/foo/bar", via: :get do
        conn.send 200, "hello world"
      end

  ## Options

  `match` accepts the following options:

  * `via:` matches the route against some specific verbs
  * `do:` contains the implementation to be invoked in case
          the route matches
  * `to:` forward the request to another module that implements
          the service/1 API

  ## Routes compilation

  All routes are compiled to a dispatch method that receives
  three arguments: the verb, the request path split on "/"
  and the connection. Consider this example:

      match "/foo/bar", via: :get do
        conn.send 200, "hello world"
      end

  It is compiled to:

      def dispatch("GET", ["foo", "bar"], conn) do
        conn.send 200, "hello world"
      end

  This opens up a few possibilities. First, guards can be given
  to match:

      match "/foo/:bar" when size(bar) <= 3, via: :get do
        conn.send 200, "hello world"
      end

  Second, a list of splitten paths (which is the compiled result)
  is also allowed:

      match ["foo", bar], via: :get do
        conn.send 200, "hello world"
      end

  """
  defmacro match(expression, options, contents // []) do
    compile(:generate_match, expression, Keyword.merge(contents, options))
  end

  @doc """
  Forwards the given route to the specified module.

  ## Examples

      forward "/foo/bar", to: Posts

  Now all the routes that start with `/foo/bar` will automatically
  be dispatched to `Posts` that needs to implement the service API.
  """
  defmacro forward(expression, options) do
    what = Keyword.get(options, :to, nil)

    unless what, do:
      raise ArgumentError, message: "Expected to: to be given to forward"

    block =
      quote do
        target = unquote(what)
        conn   = var!(conn).forward_to var!(glob), target
        target.service(conn)
      end

    options = Keyword.put(options, :do, block)
    compile(:generate_forward, expression, options)
  end

  @doc """
  Dispatches to the path only if it is get request.
  See `match/3` for more examples.
  """
  defmacro get(path, contents) do
    compile(:generate_match, path, Keyword.merge(contents, via: :get))
  end

  @doc """
  Dispatches to the path only if it is post request.
  See `match/3` for more examples.
  """
  defmacro post(path, contents) do
    compile(:generate_match, path, Keyword.merge(contents, via: :post))
  end

  @doc """
  Dispatches to the path only if it is put request.
  See `match/3` for more examples.
  """
  defmacro put(path, contents) do
    compile(:generate_match, path, Keyword.merge(contents, via: :put))
  end

  @doc """
  Dispatches to the path only if it is delete request.
  See `match/3` for more examples.
  """
  defmacro delete(path, contents) do
    compile(:generate_match, path, Keyword.merge(contents, via: :delete))
  end

  ## Match Helpers

  # Entry point for both forward and match that is actually
  # responsible to compile the route.
  defp compile(generator, expression, options) do
    verb  = Keyword.get options, :via, nil
    block = Keyword.get options, :do, nil
    to    = Keyword.get options, :to, nil

    verb_guards = convert_verbs(List.wrap(verb))
    { path, guards } = extract_path_and_guards(expression, default_guards(verb_guards))

    contents =
      cond do
        block -> block
        to    -> quote do: unquote(to).service(var!(conn))
        true  -> raise ArgumentError, message: "Expected to: or do: to be given"
      end

    match = apply Dynamo.Router.Utils, generator, [path]

    args = [
      { :_verb, 0, :quoted },
      match,
      { :conn, 0, nil }
    ]

    quote do
      def dispatch(unquote_splicing(args)) when unquote(guards) do
        var!(conn) = run_prepare_hooks(var!(conn))
        run_finalize_hooks(unquote(contents))
      end
    end
  end

  # Convert the verbs given with :via into a variable
  # and guard set that can be added to the dispatch clause.
  defp convert_verbs([]) do
    true
  end

  defp convert_verbs(raw) do
    [h|t] =
      Enum.map raw, fn(verb) ->
        verb = Dynamo.Router.Utils.normalize_verb(verb)
        quote do
          _verb == unquote(verb)
        end
      end

    Enum.reduce t, h, fn(i, acc) ->
      quote do
        unquote(acc) or unquote(i)
      end
    end
  end

  # Extract the path and guards from the path.
  defp extract_path_and_guards({ :when, _, [path, guards] }, extra_guard) do
    { path, { :and, 0, [guards, extra_guard] } }
  end

  defp extract_path_and_guards(path, extra_guard) do
    { path, extra_guard }
  end

  # Generate a default guard that is mean to avoid warnings
  # when the request or the response are not used. It
  # automatically merges the guards related to the verb.
  defp default_guards(true) do
    default_guard
  end

  defp default_guards(other) do
    { :and, 0, [other, default_guard] }
  end

  defp default_guard do
    quote hygiene: false, do: is_tuple(conn)
  end

  ## Hooks

  @doc """
  Defines a prepare hook that is executed before any route matches.
  """
  defmacro prepare(do: block) do
    quote do
      name   = :"__prepare_hook_#{length(@__prepare_hooks)}"
      args   = quote do: [var!(conn)]
      guards = quote do: [is_tuple(var!(conn))]
      defp name, args, guards, do: unquote(Macro.escape block)
      @__prepare_hooks name
    end
  end

  defmacro prepare(spec) do
    quote do
      @__prepare_hooks unquote(spec)
    end
  end

  @doc """
  Defines an after hook that is executed after dispatch.
  """
  defmacro finalize(do: block) do
    quote do
      name   = :"__finalize_hook_#{length(@__finalize_hooks)}"
      args   = quote do: [var!(conn)]
      guards = quote do: [is_tuple(var!(conn))]
      defp name, args, guards, do: unquote(Macro.escape block)
      @__finalize_hooks name
    end
  end

  defmacro finalize(spec) do
    quote do
      @__finalize_hooks unquote(spec)
    end
  end

  ## Hooks helpers

  defp compile_hook(ref, acc, kind, fun) when is_atom(ref) do
    case atom_to_binary(ref) do
      "Elixir-" <> _ ->
        call = quote(do: unquote(ref).unquote(kind)(conn))
        fun.(call, ref, acc)
      _ ->
        call = quote(do: unquote(ref).(conn))
        fun.(call, ref, acc)
    end
  end

  defp compile_hook(ref, acc, kind, fun) when is_tuple(ref) do
    if is_mod_fun?(ref, kind) do
      { mod, modfun } = ref
      call = quote(do: unquote(mod).unquote(modfun)(conn))
      fun.(call, ref, acc)
    else
      ref  = Macro.escape(ref)
      call = quote(do: unquote(ref).unquote(kind)(conn))
      fun.(call, ref, acc)
    end
  end

  defp is_mod_fun?({ mod, fun } = ref, kind) when is_atom(mod) and is_atom(fun) do
    not function_exported?(ref, kind, 1)
  end

  defp is_mod_fun?(_ref, _kind), do: false

  defp compile_prepare(call, ref, acc) do
    quote do
      case unquote(call) do
        conn when is_tuple(conn) -> unquote(acc)
        nil -> unquote(acc)
        actual -> raise Dynamo.Router.InvalidHookError, kind: :prepare, hook: unquote(ref), actual: actual
      end
    end
  end

  defp compile_finalize(call, ref, acc) do
    quote do
      case unquote(call) do
        conn when is_tuple(conn) -> unquote(acc)
        nil -> unquote(acc)
        actual -> raise Dynamo.Router.InvalidHookError, kind: :finalize, hook: unquote(ref), actual: actual
      end
    end
  end
end
