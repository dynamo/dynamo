defmodule Dynamo.Router.Base do
  @moduledoc """
  This module provides the match DSL available to dynamo routers
  and their basic structure. The defined module contains the
  following functions:

  * `service(conn)` - This is the main entry point of the module,
    as defined per the Dynamo API

  * `dispatch(method, segments, conn)` - This is the function responsible
    to dispatch the routes. It is also where callbacks and other hooks are
    added; When a router points to another router, it usually does so via
    `dispatch/3`, avoiding any unecessary overhead in `service/1`.

  * `not_found(conn)` - Customizes how `not_found` pages are handled

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__), except: [before_compile: 1, __using__: 1]

      @doc false
      def service(conn) do
        if is_binary(conn.method), do: conn = conn.method(binary_to_atom(conn.method))
        dispatch(conn.method, conn.path_info_segments, conn)
      end

      @doc false
      def not_found(conn) do
        conn.resp(404, "Not found")
      end

      defoverridable [not_found: 1, service: 1]
    end
  end

  @doc false
  defmacro before_compile(_) do
    quote do
      def dispatch(_, _, conn) do
        not_found(conn)
      end
    end
  end

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
  four arguments: the verb, the request path split on "/",
  the request and the response. Consider this example:

      match "/foo/bar", via: :get do
        conn.send 200, "hello world"
      end

  It is compiled to:

      def dispatch(:GET, ["foo", "bar"], conn) do
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

  Now all the routes that start with /foo/bar will automatically
  be dispatched to `Posts` that needs to implement the service API.
  """
  defmacro forward(expression, options) do
    what = Keyword.get(options, :to, nil)

    unless what, do:
      raise ArgumentError, "Expected to: to be given to forward"

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

  ## Helpers

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
        case run_prepare_callbacks(var!(conn)) do
          { :ok, var!(conn) }    -> run_finalize_callbacks(unquote(contents))
          { :abort, var!(conn) } -> var!(conn)
        end
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
        verb = list_to_atom(:string.to_upper(atom_to_list(verb)))
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
  # when the request or the response are not used. It automatically
  # merges the guards related to the verb.
  defp default_guards(true) do
    default_guard
  end

  defp default_guards(other) do
    { :and, 0, [other, default_guard] }
  end

  defp default_guard do
    quote hygiene: false do
      is_tuple(conn)
    end
  end
end
