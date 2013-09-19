defmodule Dynamo.Filters.Session do
  @moduledoc """
  The session filter. When added to your Dynamo, this filter allows
  you to fetch the session and to serialize it back.

  When initialized, this filter supports a set of options:

  * `key` - The name of the session cookie. This option is required;

  Besides is supports the following cookie related options:

  * `secure` - Marks the cookie as secure;

  * `domain` - The domain to which the cookie applies;

  * `path` - The path to which the cookie applies;

  * `http_only` - If the cookie is sent only via http. Default to true;

  The options above may also be set during the request, by using
  `Dynamo.HTTP.Session.configure_session/3`. For instance, to
  mark a session as secure, one can do:

      configure_session(conn, :secure, true)

  """

  defexception CookieOverflowError, message: "the session cookie exceeds the 4kb limit"

  @limit   4096
  @session :dynamo_session
  @opts    :dynamo_session_opts

  @doc false
  def new(store, opts) do
    unless key = opts[:key] do
      raise ArgumentError, message: "Expected session key to be given"
    end

    { __MODULE__, store, key, store.setup(opts) }
  end

  @doc false
  def prepare(conn, { __MODULE__, store, key, opts }) do
    conn
      .fetchable(:session, &fetch(&1, store, key, opts))
      .before_send(&serialize(&1, store, key, opts))
  end

  defp fetch(conn, store, key, opts) do
    conn = conn.fetch(:cookies)

    { id, value } =
      case conn.req_cookies[key] do
        nil    -> { nil, [] }
        cookie -> store.get_session(cookie, opts)
      end

    conn.put_private(@session, value).put_private(@opts, { id, false, [] })
  end

  defp serialize(conn, store, key, opts) do
    if session = conn.private[@session] do
      { id, written, user_opts } = conn.private[@opts]

      if user_opts[:renew] && id do
        id = nil
        written = true
        store.delete_session(id, opts)
      end

      if written do
        opts  = user_opts ++ opts
        value = store.put_session(id, session, opts)

        unless conn.req_cookies[key] == value do
          if size(value) > @limit, do: raise(CookieOverflowError)
          conn.put_resp_cookie(key, value, opts)
        end
      end
    end || conn
  end
end
