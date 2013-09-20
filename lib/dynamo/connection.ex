defmodule Dynamo.Connection do
  defrecord File, path: nil, name: nil, content_type: nil, filename: nil do
    @moduledoc """
    Contains a file representation whenever there is a multipart
    request and it contains a File.
    """
  end

  defexception UnfetchedError, aspect: nil do
    def message(exception) do
      aspect = aspect(exception)
      "did not fetch #{aspect} from request, call `conn.fetch :#{aspect}` in order to fetch it"
    end
  end

  defexception UnknownFetchError, aspect: nil do
    def message(exception) do
      "cannot fetch unknown :#{aspect(exception)}"
    end
  end

  defexception NotSentError,
    message: "no response was set in the connection"

  @moduledoc """
  This modules defines the connection API.

  By default, Elixir ships with two implementations
  of this API: `Dynamo.Cowboy.Connection` (used by
  Cowboy web server) and `Dynamo.Connection.Test`.

  Notice that this module documentation uses the record
  notation. So although the documentation says `params(conn)`,
  the function should be invoked as `conn.params()` and
  Elixir automatically moves the `conn` to the last argument.

  It is also important to remind that, as in all Elixir
  structures, a connection is immutable. So if you are using
  `conn.put_resp_header("X-API", "123456")` to set a response
  header, a new connection will be returned with the new header
  set. The original `conn` is not going to be modified.
  """

  @doc """
  Default values for before send callbacks.
  It contains callbacks to set content type,
  configure cookies and session.
  """
  def default_before_send do
    [ &put_resp_content_type_header(&1) ]
  end

  defp put_resp_content_type_header(conn) do
    if content_type = conn.resp_content_type do
      if charset = conn.resp_charset do
        content_type = content_type <> "; charset=" <> charset
      end
      conn.put_resp_header("content-type", content_type)
    else
      conn
    end
  end

  @opaque t            :: tuple
  @type   conn         :: Dynamo.Connection.t
  @type   body         :: binary
  @type   status       :: non_neg_integer
  @type   headers      :: Binary.Dict.t
  @type   method       :: binary
  @type   segments     :: [binary]
  @type   charset      :: binary
  @type   content_type :: binary
  @type   fetch_aspect :: :headers | :params | :cookies | :body | atom
  @type   main          :: module
  @type   assigns      :: Keyword.t
  @type   private      :: Keyword.t
  @type   scheme       :: :http | :https
  @type   host         :: binary
  @type   port_number  :: :inet.port_number()
  @type   host_url     :: binary
  @type   state        :: :unset | :set | :chunked | :sendfile | :sent

  use Behaviour

  ## Request API

  @doc """
  Returns the params retrieved from the query string and the request
  body as a `Binary.Dict`. The parameters need to be explicitly
  fetched with `conn.fetch(:params)` before using this function.
  """
  defcallback params(conn) :: Binary.Dict.t | no_return

  @doc """
  Returns the parameters that were set as part of the route
  matching. This is used internally by Dynamo and a developer
  likely does not need to invoke it manually.
  """
  defcallback route_params(conn) :: Keyword.t

  @doc """
  Updates the route parameters. This is used internally by Dynamo
  and a developer does not need to invoke it manually.
  """
  defcallback route_params(Keyword.t, conn) :: conn

  @doc """
  Returns the request headers as `Binary.Dict`. Note that duplicated
  entries are removed. The headers need to be explicitly fetched with
  `conn.fetch(:headers)` before using this function. Headers keys are
  all downcased.
  """
  defcallback req_headers(conn) :: headers | no_return

  @doc """
  Returns the request body as a binary.
  """
  defcallback req_body(conn) :: body | no_return

  @doc """
  The remote host ip address.
  """
  defcallback peer(conn) :: tuple

  @doc """
  Returns the HTTP method as a binary.

  ## Examples

      conn.method #=> "GET"

  """
  defcallback method(conn) :: method

  @doc """
  Returns the original HTTP method as a binary.
  Sometimes a filter may change the method from
  HEAD to GET or from POST to PUT, this function
  returns the original method.

  ## Examples

      conn.original_method #=> "GET"

  """
  defcallback original_method(conn) :: method

  @doc """
  Changes the request method to the given `method`,
  storing the previous value in original_method.
  """
  defcallback method(method, conn) :: conn

  @doc """
  Returns the HTTP version.
  """
  defcallback version(conn) :: binary

  @doc """
  Returns the request scheme.
  """
  defcallback scheme(conn) :: scheme

  @doc """
  Returns the request host.
  """
  defcallback host(conn) :: host

  @doc """
  Returns the request port.
  """
  defcallback port(conn) :: port_number

  @doc """
  Returns the host_url (i.e. containing the scheme,
  hort and port).
  """
  defcallback host_url(conn) :: host_url

  ## Paths

  @doc """
  Returns the query string as a binary.
  """
  defcallback query_string(conn) :: binary

  @doc """
  Returns the full path segments, as received by the web server.
  """
  defcallback path_segments(conn) :: [binary]

  @doc """
  Returns the full path as a binary, as received by the web server.
  """
  defcallback path(conn) :: binary

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  defcallback path_info_segments(conn) :: segments

  @doc """
  Returns the request path relative to the forwarding endpoint
  as a binary.
  """
  defcallback path_info(conn) :: binary

  @doc """
  As in CGI environment, returns the current forwarded endpoint as segments.
  """
  defcallback script_name_segments(conn) :: segments

  @doc """
  As in CGI environment, returns the current forwarded endpoint as binary.
  """
  defcallback script_name(conn) :: binary

  @doc """
  Mounts the request by setting the new path information to the given
  *segments*. Both script_name/1 and path_segments/1 are updated.
  The segments given must be a suffix of the current path segments.
  """
  defcallback forward_to(segments, module, conn) :: conn

  ## Response API

  @doc """
  Returns a boolean if the connection was already sent or not.
  This must return the correct result regardless if we have an
  updated copy of the connection or not.
  """
  defcallback already_sent?(conn) :: boolean

  @doc """
  Sends to the client the given status and body.
  An updated connection is returned with `:sent` state,
  the given status and response body set to nil.
  """
  defcallback send(status, body :: term, conn) :: conn

  @doc """
  Starts to send a chunked response to the client.
  An updated connection is returned with `:chunked` state,
  the given status and response body set to nil.
  Use `chunk/2` to chunk each part of the response.
  """
  defcallback send_chunked(status, conn) :: conn

  @doc """
  Send the given data through the socket.
  `send_chunked/2` needs to be called before `chunk/2`.
  """
  defcallback chunk(body, conn) :: { :ok, conn } | { :error, atom }

  @doc """
  Returns the response status if one was set.
  """
  defcallback status(conn) :: status

  @doc """
  Sets the response status and changes the state to `:set`.
  """
  defcallback status(status, conn) :: conn

  @doc """
  Returns the response body if one was set.
  """
  defcallback resp_body(conn) :: body | nil

  @doc """
  Sets the response body and changes the state to `:set`.
  """
  defcallback resp_body(body, conn) :: conn

  @doc """
  Gets the response charset.
  Defaults to "utf-8".
  """
  defcallback resp_charset(conn) :: binary

  @doc """
  Sets the response charset. The charset is just added
  to the response if `resp_content_type` is also set.
  """
  defcallback resp_charset(charset, conn) :: conn

  @doc """
  Gets the response content-type.
  """
  defcallback resp_content_type(conn) :: binary | nil

  @doc """
  Sets the response content-type.
  This is sent as a header when the response is sent.
  """
  defcallback resp_content_type(content_type, conn) :: conn

  @doc """
  Sets a response to the given status and body. The
  response will only be sent when `send` is called.

  After calling this function, the state changes to `:set`,
  both `status` and `resp_body` are set.
  """
  defcallback resp(status, body, conn) :: conn

  @doc """
  A shortcut to `conn.send(conn.status, conn.resp_body)`.
  """
  defcallback send(conn) :: conn

  @doc """
  Sends the file at the given path. It is expected that the
  given path exists and it points to a regular file. The
  file is sent straight away.
  """
  defcallback sendfile(status, path :: binary, conn) :: conn

  @doc """
  Returns the response state. It can be:

  * `:unset` - the response was not configured yet
  * `:set` - the response was set via `conn.resp_body` or `conn.status`
  * `:chunked` - the response is being sent in chunks
  * `:sent` - the response was sent

  """
  defcallback state(conn) :: state

  @doc """
  Returns the response headers as `Binary.Dict`.
  Header names are all downcased.
  """
  defcallback resp_headers(conn) :: Binary.Dict.t

  @doc """
  Puts a response header, overriding any previous value.
  Both `key` and `value` are converted to binary.
  The header key must be downcased.
  """
  defcallback put_resp_header(key :: String.Chars.t, value :: String.Chars.t, conn) :: conn

  @doc """
  Deletes a response header.
  The header key must be downcased.
  """
  defcallback delete_resp_header(key :: String.Chars.t, conn) :: conn

  ## Cookies

  @doc """
  Returns the cookies sent in the request as a `Binary.Dict`.
  Cookies need to be explicitly fetched with `conn.fetch(:cookies)`
  before using this function.
  """
  defcallback req_cookies(conn) :: Binary.Dict.t | no_return

  @doc """
  Returns the response cookies as a list of three element tuples
  containing the key, value and given options.
  """
  defcallback resp_cookies(conn) :: [{ binary, binary, list }]

  @doc """
  Puts the response cookie with the given key, value and list
  of options.
  """
  defcallback put_resp_cookie(key :: binary, value :: binary | nil, opts :: list, conn) :: conn

  ## Misc

  @doc """
  Responsible for fetching and caching aspects of the response.
  The default "fetchable" aspects are: headers, params, cookies and body.
  """
  defcallback fetch(fetch_aspect | [fetch_aspect], conn) :: conn

  @doc """
  Register an aspect as fetchable in the connection.
  """
  defcallback fetchable(atom, (conn -> conn), conn) :: conn

  @doc """
  Returns a keywords list with assigns set so far.
  """
  defcallback assigns(conn) :: assigns

  @doc """
  Puts a new assign with the given key and value.
  """
  defcallback assign(key :: atom, value :: term, conn) :: conn

  @doc """
  The same as `assign/3`.
  """
  defcallback put_assign(key :: atom, value :: term, conn) :: conn

  @doc """
  Returns a keywords list with private assigns set so far.
  """
  defcallback private(conn) :: assigns

  @doc %S"""
  Sets a new private assign with the given key and value.
  We recommended private keys to follow the format:

    :"#{plugin}_#{key}"

  For example, a Dynamo key to store timeout values would
  be set as: `:dynamo_timeout`.
  """
  defcallback put_private(key :: atom, value :: term, conn) :: conn

  @doc """
  Returns the main module that started the connection the request.
  """
  defcallback main(conn) :: main
end
