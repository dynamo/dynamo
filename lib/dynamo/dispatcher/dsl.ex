defmodule Dynamo::Dispatcher::DSL do
  # Main API to define routes. All three arguments are compiled
  # down to a function named `dispatch` that is executed in case
  # the verb and the path matches. The matching simply relies on
  # pattern matching and allow developers to write their own routes
  # if necessary.
  defmacro route(verb, expression, contents) do
    { path, guards } = extract_path_and_guards(expression, default_guard)

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
      match  = Dynamo::Router.generate_match unquote(path)
      args   = [quote(do: unquote(verb)), match, { :request, 0, nil }, { :response, 0, nil }]
      guards = quote hygiene: false, do: unquote(guards)
      def :dispatch, args, guards, unquote(contents)
    end
  end

  defmacro get(path, contents) do
    route(:GET, path, contents)
  end

  ## Helpers

  defp extract_path_and_guards({ :when, _, [path, guards] }, extra_guard) do
    { path, { :andalso, 0, [extra_guard, guards] } }
  end

  defp extract_path_and_guards(path, extra_guard) do
    { path, extra_guard }
  end

  # We generate this default guard to avoid warnings
  # when the request or the response are not used.
  defp default_guard do
    quote hygiene: false do
      response != nil and request != nil
    end
  end
end