defmodule Dynamo.Views do
  defrecord Template, identifier: nil, format: nil, handler: nil, source: nil, ref: nil, updated_at: nil

  defexception TemplateNotFound, query: nil, view_paths: nil do
    def message(exception) do
      "Could not find template #{inspect exception.query} in any of view paths: #{inspect exception.view_paths}"
    end
  end

  def render(query, view_paths, assigns) do
    template = Enum.find_value(view_paths, fn(x) -> x.find(query) end)
    check_template(template, query, view_paths)
    Dynamo.Views.Renderer.render(template, Keyword.put(assigns, :template, template))
  end

  defp check_template(nil, query, view_paths) do
    raise Dynamo.Views.TemplateNotFound, query: query, view_paths: view_paths
  end

  defp check_template(_, _, _) do
    :ok
  end
end