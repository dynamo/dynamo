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
  verbs are `get`, `post`, `put`, `patch`, `delete` and `options`.

  A route can also specify parameters which will then be
  available in the function body:

      get "/hello/:name" do
        conn.resp 200, "hello \#{name}"
      end

  Routes allow for globbing which will match the remaining parts
  of a route and can be available as a parameter in the function
  body, also note that a glob can't be followed by other segments:

      get "/hello/*" do
        conn.resp 200, "match all routes starting with /hello"
      end

      get "/hello/*glob" do
        conn.resp 200, "route after /hello: \#{glob}"
      end

  Finally, a general `match` function is also supported:

      match "/hello" do
        conn.resp 200, "world"
      end

  A `match` will match any route regardless of the HTTP verb.
  Check `match/3` for more information on how route compilation
  works and a list of supported options.

  ## Forwarding

  A Dynamo router can also forward a specific route to any other
  router, allowing split a Dynamo into many routers instead of
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

  This module also provides both `prepare/1` and `finalize/1` hooks to
  routers. Such hooks are useful to check conditions, fetch aspects
  or updating the connection:

      prepare do
        conn.assign :layout, "hello.html"
      end

      prepare :require_authentication

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

  ## Per-route hooks

  Besides the hooks define above which runs right after any route match,
  per route hooks can be added using `@prepare` and `@finalize` attributes:

      @prepare :require_authentication
      get "/account/info" do
        # ...
      end

  Annotations works similarly to `prepare` and `finalize` macros, except
  annotations do not support the `do` syntax.

  Finally, Dynamo also provides `@skip_prepare` and `@skip_finalize`
  attributes, which is useful for whitelisting some filters:

      # Require authentication for all routes
      prepare :require_authentication

      # Except the sign in route
      @skip_prepare :require_authentication
      get "/sign_in" do
        # Sign in the user
      end

  """

  @doc false
  defmacro __using__(_) do
    [ quote do
        Enum.each [:prepare, :finalize, :skip_prepare, :skip_finalize],
          &Module.register_attribute(__MODULE__, &1, accumulate: true, persist: false)
        @before_compile unquote(__MODULE__)
        @dynamo_prepare []
        @dynamo_finalize []

        require Bitwise
        import unquote(__MODULE__)
      end,

      quote location: :keep do
        @doc false
        def service(conn) do
          dispatch(conn.method, conn.path_info_segments, conn)
        end

        defoverridable [service: 1]
      end ]
  end

  @doc false
  defmacro __before_compile__(env) do
    module = env.module

    prepare = compile_hooks module, :dynamo_prepare,
                 quote(do: var!(conn)), :prepare, &compile_prepare/4, true

    finalize = compile_hooks module, :dynamo_finalize,
                 quote(do: var!(conn)), :finalize, &compile_finalize/4, true

    [ quote location: :keep do
        @doc false
        def run_prepare_hooks(var!(conn), key),  do: unquote(prepare)

        @doc false
        def run_finalize_hooks(var!(conn), key), do: unquote(finalize)
      end,

      quote do
        @doc false
        match _ do
          raise Dynamo.NotFoundError, conn: var!(conn)
        end
      end ]
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
      raise(ArgumentError, message: "Expected to: to be given to forward")

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
  Dispatches to the path only if it is patch request.
  See `match/3` for more examples.
  """
  defmacro patch(path, contents) do
    compile(:generate_match, path, Keyword.merge(contents, via: :patch))
  end

  @doc """
  Dispatches to the path only if it is delete request.
  See `match/3` for more examples.
  """
  defmacro delete(path, contents) do
    compile(:generate_match, path, Keyword.merge(contents, via: :delete))
  end

  @doc """
  Dispatches to the path only if it is options request.
  See `match/3` for more examples.
  """
  defmacro options(path, contents) do
    compile(:generate_match, path, Keyword.merge(contents, via: :options))
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
        Keyword.has_key?(options, :do) -> block
        to -> quote do: unquote(to).service(var!(conn))
        true -> raise ArgumentError, message: "Expected :to or :do to be given as option"
      end

    { vars, match } = apply Dynamo.Router.Utils, generator, [path]
    args  = quote do: [_verb, unquote(match), var!(conn)]

    quote bind_quoted: [args: Macro.escape(args),
                        guards: Macro.escape(guards),
                        vars: Macro.escape(vars),
                        contents: Macro.escape(contents)] do
      body = Dynamo.Router.Base.__hooks__(__MODULE__, vars, contents)
      def dispatch(unquote_splicing(args)) when unquote(guards), do: unquote(body)
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
    { path, { :and, [], [guards, extra_guard] } }
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
    { :and, [], [other, default_guard] }
  end

  defp default_guard do
    quote do: is_tuple(var!(conn))
  end

  ## Hooks

  @doc """
  Defines a prepare hook that is executed before any route matches.
  """
  defmacro prepare(do: block) do
    quote bind_quoted: [block: Macro.escape(block)] do
      name = :"__dynamo_prepare_hook_#{length(@dynamo_prepare)}"
      defp unquote(name)(var!(conn)) when is_tuple(var!(conn)), do: unquote(block)
      @dynamo_prepare [{ name, nil }|@dynamo_prepare]
    end
  end

  defmacro prepare(spec) do
    quote do
      @dynamo_prepare [{ unquote(spec), nil }|@dynamo_prepare]
    end
  end

  @doc """
  Defines an after hook that is executed after dispatch.
  """
  defmacro finalize(do: block) do
    quote bind_quoted: [block: Macro.escape(block)] do
      name  = :"__dynamo_finalize_hook_#{length(@dynamo_finalize)}"
      defp unquote(name)(var!(conn)) when is_tuple(var!(conn)), do: unquote(block)
      @dynamo_finalize [{ name, nil }|@dynamo_finalize]
    end
  end

  defmacro finalize(spec) do
    quote do
      @dynamo_finalize [{ unquote(spec), nil }|@dynamo_finalize]
    end
  end

  ## Hooks helpers

  # Used to retrieve hooks at function definition.
  @doc false
  def __hooks__(module, vars, contents) do
    hooks = compile_hooks module, :finalize, quote(do: var!(conn)),
              :finalize, &compile_finalize/4, false

    hooks = quote do
      var!(conn) = unquote(contents)
      unquote(hooks)
    end

    unless vars == [] do
      route_params = lc var inlist vars, do: { var, { var, [], nil } }
      hooks = quote do
        var!(conn) = var!(conn).route_params(unquote(route_params))
        unquote(hooks)
      end
    end

    hooks = compile_hooks module, :prepare, hooks,
              :prepare, &compile_prepare/4, false

    prepare_key  = compile_skip_hooks module, :skip_prepare,  :dynamo_prepare
    finalize_key = compile_skip_hooks module, :skip_finalize, :dynamo_finalize

    hooks =
      quote do
        var!(conn) = run_prepare_hooks(var!(conn), unquote(prepare_key))
        run_finalize_hooks(unquote(hooks), unquote(finalize_key))
      end

    Enum.each [:prepare, :finalize, :skip_prepare, :skip_finalize],
      &Module.delete_attribute(module, &1)
    hooks
  end

  # The boolean argument is if the set of hooks are
  # tuples with a mod value or not. The mod values
  # are a bit system to check if a hook should be
  # run or not.
  defp compile_hooks(module, attr, acc, kind, fun, true) do
    hooks = Module.get_attribute(module, attr)
    Enum.reduce hooks, acc, fn({ hook, key }, acc) ->
      compile_hook(hook, key, acc, kind, fun)
    end
  end

  defp compile_hooks(module, attr, acc, kind, fun, false) do
    hooks = Module.get_attribute(module, attr)
    Enum.reduce hooks, acc, fn(hook, acc) ->
      compile_hook(hook, nil, acc, kind, fun)
    end
  end

  defp compile_hook(ref, key, acc, kind, fun) when is_atom(ref) do
    case atom_to_binary(ref) do
      "Elixir." <> _ ->
        call = quote(do: unquote(ref).unquote(kind)(var!(conn)))
        fun.(call, key, ref, acc)
      _ ->
        call = quote(do: unquote(ref)(var!(conn)))
        fun.(call, key, ref, acc)
    end
  end

  defp compile_hook(ref, key, acc, kind, fun) when is_tuple(ref) do
    if is_mod_fun?(ref, kind) do
      { mod, modfun } = ref
      call = quote(do: unquote(mod).unquote(modfun)(var!(conn)))
      fun.(call, key, ref, acc)
    else
      ref  = Macro.escape(ref)
      call = quote(do: unquote(ref).unquote(kind)(var!(conn)))
      fun.(call, key, ref, acc)
    end
  end

  defp is_mod_fun?({ mod, fun } = ref, kind) when is_atom(mod) and is_atom(fun) do
    not function_exported?(ref, kind, 1)
  end

  defp is_mod_fun?(_ref, _kind), do: false

  defp compile_prepare(call, key, ref, acc) do
    quote do
      case unquote(compile_condition(call, key)) do
        var!(conn) when is_tuple(var!(conn)) -> unquote(acc)
        x when x in [nil, false] -> unquote(acc)
        actual -> raise Dynamo.Router.InvalidHookError, kind: :prepare, hook: unquote(ref), actual: actual
      end
    end
  end

  defp compile_finalize(call, key, ref, acc) do
    quote do
      case unquote(compile_condition(call, key)) do
        var!(conn) when is_tuple(var!(conn)) -> unquote(acc)
        x when x in [nil, false] -> unquote(acc)
        actual -> raise Dynamo.Router.InvalidHookError, kind: :finalize, hook: unquote(ref), actual: actual
      end
    end
  end

  defp compile_condition(call, nil), do: call

  defp compile_condition(call, key) do
    quote do: Bitwise.band(unquote(key), key) == 0 and unquote(call)
  end

  # Compile skip hooks. If any skip hook was given, we
  # search for it in the chain, add an index if there
  # isn't one yet and add it to the key.
  defp compile_skip_hooks(module, skip, chain) do
    hooks = Module.get_attribute(module, skip)
    Enum.reduce hooks, 0, fn(hook, acc) ->
      acc + compile_skip_hook(module, chain, hook)
    end
  end

  defp compile_skip_hook(module, chain, name) do
    hooks = Module.get_attribute(module, chain)
    hook  = Enum.find(hooks, &match?({ ^name, _ }, &1))

    case hook do
      { _, nil } ->
        # Hook does not have a key, calculate a new one
        # and update the hook chain to contain it
        calculate_skip_hook_key(module, chain, hooks, name)
      { _, key } ->
        key
      nil ->
        raise ArgumentError, message: "Could not skip hook #{inspect name} because it could not be found in this router"
    end
  end

  defp calculate_skip_hook_key(module, chain, hooks, name) do
    key =
      Enum.reduce hooks, 1, fn
        { _, nil }, acc -> acc
        _, acc -> acc * 2
      end

    hooks =
      Enum.map hooks, fn hook ->
        case hook do
          { ^name, _ } -> { name, key }
          other -> other
        end
      end

    Module.put_attribute(module, chain, hooks)
    key
  end
end
