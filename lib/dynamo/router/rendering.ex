defmodule Dynamo.Router.Rendering do
  @moduledoc """
  Module responsible for template rendering
  and similar functions.
  """

  @doc """
  Renders a template and assigns its contents
  to the given `conn`. The template content type,
  if available, is also set in the `conn`.

  Raises Dynamo.View.TemplateNotFound if the given
  template can't be found.
  """
  def render(conn, template) do
    view_paths = Dynamo.app.view_paths
    template   = Dynamo.View.find(template, view_paths)

    unless template do
      raise Dynamo.View.TemplateNotFound, query: template, view_paths: view_paths
    end

    if template.format && !conn.resp_content_type do
      mime = :mimetypes.ext_to_mimes(template.format)
      conn = conn.resp_content_type(hd(mime))
    end

    body = Dynamo.View.render(template, [conn: conn], conn.assigns)
    conn.resp_body(body)
  end  
end