defmodule Dynamo.Router.Session do
  @doc """
  Conveniences for working with session.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__)
    end
  end

  @session :dynamo_session

  @doc """
  Gets the whole session.
  """
  def get_session(conn) do
    case Keyword.get(conn.private, @session) do
      nil   -> raise Dynamo.HTTP.UnfetchedError, aspect: :session
      other -> other
    end
  end

  @doc """
  Returns the session for the given key.
  """
  def get_session(conn, key) do
    case List.keyfind(get_session(conn), key, 0, nil) do
      { ^key, value } -> value
      nil -> nil
    end
  end

  @doc """
  Sets the session for the given key.
  """
  def put_session(conn, key, value) do
    session = [{ key, value }|List.keydelete(get_session(conn), key, 0)]
    conn.private(@session, session)
  end
end