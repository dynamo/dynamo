defmodule Dynamo.View do
  defrecord Template, key: nil, identifier: nil, format: nil,
      handler: nil, source: nil, updated_at: nil, ref: nil do
    @moduledoc """
    The template record is responsible to keep information about
    templates to be rendered. It contains:

    * `:key` - The key used to find the template
    * `:identifier` - An unique identifier for the template, like its filesystem path
    * `:format` - The template format
    * `:handler` - The handler responsible for compiling the template
    * `:source` - The template source code
    * `:updated_at` - The last time the template was updated
    * `:ref` - A reference for already compiled templates
    """
  end

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