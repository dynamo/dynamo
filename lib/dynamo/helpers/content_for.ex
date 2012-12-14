defmodule Dynamo.Helpers.ContentFor do
  @moduledoc """
  This module exposes macros that allows a child
  template to assign data which will eventually
  be read and use by the parent.

  ## Examples

  Imagine the following layout:

      <html>
      <head>
        <title>
          <%= content_for(:title) %>
        </title>
      </head>
      <body>
        <%= content_for(:template) %>
        <%= content_for(:footer) || "Default footer" %>
      </body>
      </html>

  And the following template:

      <% content_for :title, fn -> %>
        Title from template
      <% end %>

      Template body.
      This is returned by content_for :template.

      <% content_for :footer, "Template footer" %>

  Whenever this template and layout pair are used,
  the template is first rendered, collecting the
  `content_for` chunks used and then finally assigning
  the whole template to a `:template` chunk. The layout
  can later retrieve any chunk by calling `content_for(key)`.
  """

  @key :dynamo_contents

  @doc """
  Stores the given `value` under the given `key`. This value
  can later be retrieved by calling `content_for(key)`.

  ## Implementation details

  Whenever `content_for` is called, the contents are stored
  in `conn`, which is then reassigned. The connection is passed
  transparently to `content_for` via a macro. The decision to make
  this transparent is because different templates implementations
  may use other mechanisms to pass the data around, which does
  not require mangling with the connection.
  """
  defmacro content_for(key, value) do
    quote hygiene: false do
      conn = unquote(__MODULE__).put_content(conn, unquote(key), unquote(value))
    end
  end

  @doc """
  Gets the content for the given key. If the stored value
  is a function, it is automatically invoked, otherwise
  returns the raw value.
  """
  defmacro content_for(key) do
    quote hygiene: false do
      unquote(__MODULE__).get_content(conn, unquote(key))
    end
  end

  @doc """
  A simple function that stores a content chunk in the connection.
  """
  def put_content(conn, key, value) when is_atom(key) and (is_binary(value) or is_function(value)) do
    value = Keyword.put(conn.private[@key] || [], key, value)
    conn.put_private(@key, value)
  end

  @doc """
  A simple function that reads a content chunk from the connection.
  """
  def get_content(conn, key) when is_atom(key) do
    contents = conn.private[@key][key]
    if is_function(contents), do: contents.(), else: contents
  end
end