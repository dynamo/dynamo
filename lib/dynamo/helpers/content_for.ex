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

      <% content_for :title do %>
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

  ## Implementation details

  Whenever `content_for` is called, the contents are stored
  in `conn`, which is then reassigned. The connection is
  passed transparently to `content_for` via a macro. The
  decision to make this transparent is because different
  templates implementations may use other mechanisms to pass
  the data around, which does not require mangling with the
  connection.

  Manual interaction with the connection can be done via
  `append_content` and `get_content` functions.
  """

  @key :dynamo_contents

  @doc """
  Stores the given `value` under the given `key`. This value
  can later be retrieved by calling `content_for(key)`.
  """
  defmacro content_for(key, do: value) do
    quote do
      var!(conn) = unquote(__MODULE__).append_content(var!(conn), unquote(key), unquote(value))
    end
  end

  defmacro content_for(key, value) do
    quote do
      var!(conn) = unquote(__MODULE__).append_content(var!(conn), unquote(key), unquote(value))
    end
  end

  @doc """
  Gets the content for the given key. If the stored value
  is a function, it is automatically invoked, otherwise
  returns the raw value.
  """
  defmacro content_for(key) do
    quote do
      unquote(__MODULE__).get_content(var!(conn), unquote(key))
    end
  end

  @doc """
  Appends a content chunk to the connection.
  """
  def append_content(conn, key, value) when is_atom(key) and is_binary(value) do
    value = Keyword.update(conn.private[@key] || [], key, value, &(&1 <> value))
    conn.put_private(@key, value)
  end

  @doc """
  Puts a content chunk to the connection replacing previous entries.
  """
  def put_content(conn, key, value) when is_atom(key) and is_binary(value) do
    value = Keyword.put(conn.private[@key] || [], key, value)
    conn.put_private(@key, value)
  end

  @doc """
  Gets a content chunk from the connection.
  """
  def get_content(conn, key) when is_atom(key) do
    conn.private[@key][key]
  end
end
