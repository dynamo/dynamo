defmodule Dynamo.Views do
  defrecord Template, identifier: nil, format: nil, handler: nil, source: nil, updated_at: nil

  defexception TemplateNotFound, query: nil, view_paths: nil do
    def message(exception) do
      "Could not find template #{inspect exception.query} in any of view paths: #{inspect exception.view_paths}"
    end
  end

  def render(query, view_paths, assigns) do
    template = Enum.find_value(view_paths, fn(x) -> x.find(query) end)
    module   = compile_template template, query, view_paths

    try do
      module.render(assigns, template)
    after
      :code.purge(module)
      :code.delete(module)
    end
  end

  defp compile_template(nil, query, view_paths) do
    raise Dynamo.Views.TemplateNotFound, query: query, view_paths: view_paths
  end

  defp compile_template(Dynamo.Views.Template[handler: handler] = template, _, _) do
    source = get_handler(handler).compile(template)

    source = quote hygiene: false do
      _ = assigns
      _ = template
      unquote(source)
    end

    defmodule Elixir.Dynamo.Views.CompiledTemplate do
      args = quote hygiene: false, do: [assigns, template]
      def :render, args, [], do: source
    end

    Dynamo.Views.CompiledTemplate
  end

  # TODO: Remove hardcoded handler.
  defp get_handler("eex") do
    Dynamo.Views.EEXHandler
  end
end