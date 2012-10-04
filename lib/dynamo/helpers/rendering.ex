defmodule Dynamo.Helpers.Rendering do
  @moduledoc """
  Conveniences for rendering a template from
  inside a view.
  """

  @doc """
  Renders a template and returns the rendered
  template as a binary.

  ## Examples

      <%= render "sidebar.html", header: "Hello" %>

  """
  defmacro render(template, assigns // []) do
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
    app        = conn.app
    view_paths = app.view_paths
    prelude    = fn -> app.views end
    template   = Dynamo.View.find!(template, view_paths)

    layout  = assigns[:layout]
    assigns = Keyword.merge(conn.assigns, assigns)

    { [conn], body } = Dynamo.View.render(template, [conn: conn], assigns, prelude)

    if layout do
      template         = Dynamo.View.find!("layouts/" <> layout, view_paths)
      { [conn], body } = Dynamo.View.render(template, [conn: conn], assigns, prelude)
    end

    { conn, body }
  end
end