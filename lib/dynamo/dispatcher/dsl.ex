defmodule Dynamo::Dispatcher::DSL do
  defmacro route(verb, expression, contents) do
    { path, guards } = extract_path_and_guards(expression, default_guard)

    tail = quote hygiene: false do
      [request, response]
    end

    quote do
      match = Dynamo::Router.generate_match to_char_list(unquote(path))
      def :dispatch, [unquote(verb), match.segments | unquote(tail)], unquote(guards), unquote(contents)
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

  defp default_guard do
    quote hygiene: false do
      is_tuple(request) andalso is_tuple(response)
    end
  end
end