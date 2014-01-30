defmodule Dynamo.Helpers.Rendering do
  @moduledoc """
  Conveniences for rendering a template from
  inside another template.
  """

  @doc """
  Renders a template and returns the rendered
  template as a binary.

  ## Examples

      <%= render "sidebar.html", header: "Hello" %>

  """
  defmacro render(template, assigns \\ []) do
    quote do
      { var!(conn), body } = unquote(__MODULE__).render(var!(conn), unquote(template), unquote(assigns))
      body
    end
  end

  @doc """
  Renders a template by passing a connection.
  It should be used whenever there is a need
  to render a template inside a helper.
  """
  def render(conn, template, assigns) do
    dynamo     = conn.main
    renderer   = dynamo.templates_server
    tmpl_paths = dynamo.templates_paths
    prelude    = fn -> dynamo.templates_prelude end
    template   = Dynamo.Templates.find!(template, tmpl_paths)

    layout  = assigns[:layout]
    assigns = Keyword.merge(conn.assigns, assigns)

    { [conn], body } = Dynamo.Templates.render(renderer, template, [conn: conn], assigns, prelude)

    if layout && (layout = Dynamo.Templates.find_layout(layout, template, tmpl_paths)) do
      previous = Dynamo.Helpers.ContentFor.get_content(conn, :template)
      conn = Dynamo.Helpers.ContentFor.put_content(conn, :template, body)
      { [conn], body } = Dynamo.Templates.render(renderer, layout, [conn: conn], assigns, prelude)
      conn = Dynamo.Helpers.ContentFor.put_content(conn, :template, previous)
    end

    { conn, body }
  end
end