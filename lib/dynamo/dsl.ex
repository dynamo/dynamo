defmodule Dynamo::DSL do
  defmacro route(verb, path, contents) do
    list_path = to_list(path)
    bin_path  = to_binary(path)

    quote do
      path   = unquote(list_path)
      routes = Orddict.fetch Module.read_data(__MODULE__), :routes, []
      name   = :"_action_#{unquote(bin_path)}_#{length(routes)}"
      Module.merge_data __MODULE__, routes: [{ path, {unquote(verb), name} }|routes]
      def name, [request, response], [], unquote(contents)
    end
  end

  defmacro get(path, contents) do
    route(:GET, path, contents)
  end
end