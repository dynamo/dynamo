defmodule Dynamo.Filters.Session do
  @moduledoc """
  The session filter. When added to your application,
  this filter allows you to fetch the session and to
  serialize it back.
  """

  @todo "Add a secret"
  defmodule CookieStore do
    def get_session(nil, _opts) do
      { nil, [] }
    end

    def get_session(content, _opts) do
      { nil, content /> :base64.decode /> binary_to_term }
    end

    def put_session(_id, term, _opts) do
      term /> term_to_binary /> :base64.encode
    end
  end

  @session :dynamo_session
  @opts    :dynamo_session_opts

  def new(store, opts) do
    unless key = opts[:key] do
      raise ArgumentError, message: "Expected session key to be given"
    end

    { __MODULE__, store, key, opts }
  end

  def prepare(conn, { __MODULE__, store, key, opts }) do
    conn
      .fetchable(:session, fetch(&1, store, key, opts))
      .before_send(serialize(&1, store, key, opts))
  end

  defp fetch(conn, store, key, opts) do
    conn = conn.fetch(:cookies)
    { id, value } = store.get_session(conn.req_cookies[key], opts)
    conn.private(@session, value).private(@opts, [id: id])
  end

  @todo "Check if the session was writen besides loaded"
  defp serialize(conn, store, key, opts) do
    if session = conn.private[@session] do
      id    = conn.private[@opts][:id]
      value = store.put_session(id, session, opts)

      unless conn.req_cookies[key] == value do
        conn.put_resp_cookie(key, value, [])
      end
    end || conn
  end
end
