defmodule Dynamo.View do
  defrecord Template, identifier: nil, format: nil, handler: nil, source: nil, ref: nil, updated_at: nil

  defexception TemplateNotFound, query: nil, view_paths: nil do
    def message(exception) do
      "Could not find template #{inspect exception.query} in any of view paths: #{inspect exception.view_paths}"
    end
  end

  @doc """
  Finds the given template in any of the views paths.
  """
  def find(query, view_paths) do
    Enum.find_value(view_paths, fn(x) -> x.find(query) end)
  end

  @doc """
  Renders the given template with the given assigns.
  """
  def render(template, assigns) do
    Dynamo.View.Renderer.render(template, Keyword.put(assigns, :template, template))
  end
end