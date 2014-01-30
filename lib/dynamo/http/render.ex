defmodule Dynamo.HTTP.Render do
  @moduledoc """
  Conveniences for template rendering.
  To use them, just import this module.
  """

  @doc """
  Renders a template and assigns its contents to
  the connection response body and content type.
  If the connection is in streaming mode, the template
  is streamed after it is rendered as a whole chunk.

  Besides the connection and the template name, this function
  also receives extra assigns as arguments. Assigns are used
  by the developer to pass information from the router to the
  template.

  It raises `Dynamo.TemplateNotFound` if the given
  template can't be found.

  ## Examples

      # Renders the template usually at web/templates/hello.html
      render conn, "hello.html"

      # Assign to data (accessible as @data in the template)
      conn = conn.assign(:data, "Sample")
      render conn, "hello.html"

      # Same as before, but does not assign to the connection
      render conn, "hello.html", data: "Sample"

  ## Layouts

  Rendering also supports layouts. The layout name should
  be given as an assign. After the template is found, a layout
  with the same format will be looked up and rendered if
  available.

  It is common to set a layout that is used throughout the
  dynamo in your `ApplicationRouter` and it will be carried
  out to all other routers:

      prepare do
        conn.assign :layout, "application"
      end

  """
  def render(conn, template, assigns \\ []) do
    dynamo     = conn.main
    renderer   = dynamo.templates_server
    tmpl_paths = dynamo.templates_paths
    prelude    = fn -> dynamo.templates_prelude end
    template   = Dynamo.Templates.find!(template, tmpl_paths)
    format     = template.format

    if format && !conn.resp_content_type do
      type = MIME.Types.type(format)
      conn = conn.resp_content_type(type)
    end

    assigns = Keyword.merge(conn.assigns, assigns)
    layout  = assigns[:layout]
    { [conn], body } = Dynamo.Templates.render(renderer, template, [conn: conn], assigns, prelude)

    if layout && (layout = Dynamo.Templates.find_layout(layout, template, tmpl_paths)) do
      conn = Dynamo.Helpers.ContentFor.put_content(conn, :template, body)
      { [conn], body } = Dynamo.Templates.render(renderer, layout, [conn: conn], assigns, prelude)
    end

    conn.resp_body(body)
  end
end
