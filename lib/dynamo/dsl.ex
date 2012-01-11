defmodule Dynamo::DSL do
  defmacro route(verb, path, contents) do
    quote do
      _path      = unquote(path)
      _list_path = to_list(_path)
      _bin_path  = to_binary(_path)

      _routes = Module.read_data(__MODULE__, :routes)
      _name   = :"_action_#{_bin_path}_#{length(_routes)}"

      Module.merge_data __MODULE__, routes: [{ _list_path, {unquote(verb), _name} }|_routes]
      def _name, [var!(request), var!(response)], [is_tuple(var!(request)) and is_tuple(var!(response))], unquote(contents)
    end
  end

  defmacro get(path, contents) do
    route(:GET, path, contents)
  end
end