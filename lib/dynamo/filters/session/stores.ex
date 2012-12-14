alias Dynamo.Filters.Session

defmodule Session.Store do
  @moduledoc """
  This module defines some conveniences and the API
  required by session stores.
  """

  use Behaviour

  @type sid       :: binary
  @type maybe(v)  :: v | nil
  @type opts      :: Keyword.t
  @type cookie    :: binary
  @type session   :: list

  @doc """
  A callback invoked when the filter is first created.
  """
  defcallback setup(opts) :: opts

  @doc """
  `get_session/2` receives a binary representing the
  current cookie value and must return a tuple with
  the session id (if one exists) and the session value
  (an empty list if there is no session).
  """
  defcallback get_session(cookie, opts) :: { maybe(sid), session }

  @doc """
  `put_session/3` receives the session id and the session
  and must persist it. It must return the cookie value (which
  will be passed to `get_session/2` in the next request).

  The session id **may** be nil. In such cases, the store
  is responsible for generating a session id if required.
  """
  defcallback put_session(maybe(sid), session, opts) :: cookie

  @doc """
  `delete_session/2` is responsible for deleting a previously
  stored sid from the store.
  """
  defcallback delete_session(sid, opts) :: term
end

defmodule Session.CookieStore do
  @moduledoc """
  Implements a cookie store. This cookie store is based on
  `Dynamo.Utils.MessageVerifier` which signs each cookie to
  ensure they won't be tampered with.

  Notice the cookie contents are still visible and therefore
  private data should never be put into such store.
  """

  alias Dynamo.Utils.MessageVerifier

  @behaviour Session.Store

  def setup(opts) do
    secret = opts[:secret]

    cond do
      nil?(secret)      -> raise ArgumentError, message: "CookieStore expects a secret as option"
      size(secret) < 64 -> raise ArgumentError, message: "CookieStore secret must be at least 64 bytes"
      true              -> opts
    end
  end

  def get_session(content, opts) do
    case MessageVerifier.verify opts[:secret], content do
      { :ok, value } -> { nil, value }
      :error         -> { nil, [] }
    end
  end

  def put_session(_id, term, opts) do
    MessageVerifier.generate opts[:secret], term
  end

  def delete_session(_id, _opts) do
    :ok
  end
end
