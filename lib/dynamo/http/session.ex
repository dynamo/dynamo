defmodule Dynamo.HTTP.Session do
  @doc """
  Conveniences for working with session.
  To use them, just import this module.
  """

  @session :dynamo_session
  @opts    :dynamo_session_opts

  @doc """
  Gets the whole session.
  """
  def get_session(conn) do
    get_from_private(conn.private, @session)
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
  Removes the session for the given key
  """
  def delete_session(conn, key) do    
    private = conn.private
    session = List.keydelete(get_session(conn), key, 0)
    mark_as_writen conn.put_private(@session, session), private
  end

  @doc """
  Puts the session for the given key.
  """
  def put_session(conn, key, value) do
    private = conn.private
    session = List.keystore(get_from_private(private, @session), key, 0, { key, value })
    mark_as_writen conn.put_private(@session, session), private
  end

  @doc """
  Configure the session.
  """
  def configure_session(conn, key, value) do
    { id, writen, opts } = get_from_private(conn.private, @opts)
    conn.put_private @opts, { id, writen, Keyword.put(opts, key, value) }
  end

  defp mark_as_writen(conn, private) do
    case Keyword.get(private, @opts) do
      { _, true, _ }  -> conn
      { id, _, opts } -> conn.put_private(@opts, { id, true, opts })
    end
  end

  defp get_from_private(private, key) do
    case Keyword.get(private, key) do
      nil   -> raise Dynamo.Connection.UnfetchedError, aspect: :session
      other -> other
    end
  end
end