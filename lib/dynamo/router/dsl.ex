defmodule Dynamo::Router::DSL do
  # Main API to define routes. It accepts an expression representing
  # the path and many options allowing the match to be configured.
  #
  # ## Examples
  #
  #     match "/foo/bar", via: :get do
  #       request.ok("hello world")
  #     end
  #
  # ## Options
  #
  # `match` accepts the following options:
  #
  # * `via:` matches the route against some specific verbs
  # * `do:` contains the implementation to be invoked in case
  #         the route matches
  #
  # ## Compilation
  #
  # All routes are compiled to a dispatch method that receives
  # four arguments: the verb, the request path split on "/",
  # the request and the response. Consider this example:
  #
  #     match "/foo/bar", via: :get do
  #       request.ok("hello world")
  #     end
  #
  # It is compiled to:
  #
  #     def dispatch(:GET, ["foo", "bar"], request, response) do
  #       request.ok("hello world")
  #     end
  #
  # This opens up a few possibilities. First, guards can be given
  # to match:
  #
  #     match "/foo/:bar" when size(bar) <= 3, via: :get do
  #       request.ok("hello world")
  #     end
  #
  # Second, a list of splitten paths (which is the compiled result)
  # is also allowed:
  #
  #     match ["foo", bar], via: :get do
  #       request.ok("hello world")
  #     end
  #
  defmacro match(expression, options, contents // []) do
    options = Orddict.merge(contents, options)

    verb  = Orddict.get options, :via, nil
    block = Orddict.get options, :do, nil

    { verb_var, verb_guards } = convert_verbs(List.wrap(verb))
    { path, guards } = extract_path_and_guards(expression, default_guards(verb_guards))

    # This is a bit tricky. We use the dynamic function definition
    # so we can generate a function completely at runtime. This allow
    # us to define the router without worryting about intermediate steps.
    #
    # However, it comes with the downside that the name, arguments and
    # guards for the function needs to return their quoted expression.
    # This forces us to quote the injected guards so we can pass them
    # as arguments to def. Also notice we need to disable hygiene when
    # quoting the guards because we want the expressions inside the
    # guard to still be available in the function body.
    quote do
      match  = Dynamo::Router::Utils.generate_match unquote(path)
      args   = [quote(do: unquote(verb_var)), match, { :request, 0, nil }, { :response, 0, nil }]
      guards = quote hygiene: false, do: unquote(guards)
      def :dispatch, args, guards, do: unquote(block)
    end
  end

  # Dispatches to the path only if it is get request.
  # See `match/3` for more examples.
  defmacro get(path, contents) do
    match path, Orddict.merge(contents, via: :get)
  end

  ## Helpers

  # Convert the verbs given with :via into a variable
  # and guard set that can be added to the dispatch clause.
  defp convert_verbs([]) do
    { quote(do: _), true }
  end

  defp convert_verbs(raw) do
    var = quote(do: __verb__)

    [h|t] =
      Enum.map raw, fn(verb) ->
        verb = list_to_atom(:string.to_upper(atom_to_list(verb)))
        quote do
          unquote(var) == unquote(verb)
        end
      end

    guards =
      Enum.reduce t, h, fn(i, acc) ->
        quote do
          unquote(acc) orelse unquote(i)
        end
      end

    { var, guards }
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
      is_tuple(request) and is_tuple(response)
    end
  end
end