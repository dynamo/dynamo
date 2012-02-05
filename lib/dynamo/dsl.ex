defmodule Dynamo::DSL do
  defmacro route(verb, path, contents) do
    quote do
      _path      = unquote(path)
      _list_path = to_char_list(_path)
      _bin_path  = to_binary(_path)

      _routes = @routes
      _name   = :"_action_#{_bin_path}_#{length(_routes)}"

      @routes [{ _list_path, {unquote(verb), _name} }|_routes]
      def _name, [var!(request), var!(response)], is_tuple(var!(request)) & is_tuple(var!(response)), unquote(contents)
    end
  end

  defmacro get(path, contents) do
    route(:GET, path, contents)
  end
end