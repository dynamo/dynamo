defmodule Dynamo.HTTP do
  defrecord File, body: nil, name: nil, content_type: nil, filename: nil do
    @moduledoc """
    Contains a file representation whenever there is a multipart
    request and it contains a File.
    """
  end

  defexception UnfetchedError, aspect: nil do
    def message(exception) do
      aspect = aspect(exception)
      "Did not fetch #{aspect} from request, add `fetch :#{aspect}` in order to access it"
    end
  end

  defexception InvalidSendOnHeadError, message: "Cannot send data because conn.original_method is HEAD"

  use Behaviour

  ## Request API

  @doc """
  Returns the params retrieved from the query string and the request
  body as a `Binary.Dict`. The parameters need to be explicitly
  fetched with `conn.fetch(:params)` before using this function.
  """
  defcallback params(conn)

  @doc """
  Returns the request headers as `Binary.Dict`. Note that duplicated
  entries are removed. The headers need to be explicitly fetched with
  `conn.fetch(:headers)` before using this function.
  """
  defcallback req_headers(conn)

  @doc """
  Returns the HTTP method as an atom.

  ## Examples

      conn.method #=> "GET"

  """
  defcallback method(conn)

  @doc """
  Returns the original HTTP method as an atom.
  Sometimes a filter may change the method from
  HEAD to GET or from POST to PUT, this function
  returns the original method.

  ## Examples

      conn.original_method #=> "GET"

  """
  defcallback original_method(conn)

  @doc """
  Changes the request method to the given `method`,
  storing the previous value in original_method.
  """
  defcallback method(method, conn)

  @doc """
  Returns the HTTP version.
  """
  defcallback version(conn)

  ## Paths

  @doc """
  Returns the query string as a binary.
  """
  defcallback query_string(conn)

  @doc """
  Returns the full path segments, as received by the web server.
  """
  defcallback path_segments(conn)

  @doc """
  Returns the full path as a binary, as received by the web server.
  """
  defcallback path(conn)

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  defcallback path_info_segments(conn)

  @doc """
  Returns the request path relative to the forwarding endpoint
  as a binary.
  """
  defcallback path_info(conn)

  @doc """
  As in CGI environment, returns the current forwarded endpoint as segments.
  """
  defcallback script_name_segments(conn)

  @doc """
  As in CGI environment, returns the current forwarded endpoint as binary.
  """
  defcallback script_name(conn)

  @doc """
  Mounts the request by setting the new path information to the given
  *segments*. Both script_name/1 and path_segments/1 are updated.
  The segments given must be a suffix of the current path segments.
  """
  defcallback forward_to(segments, target, conn)

  ## Response API

  @doc """
  Sends to the client the given status and body.
  An updated connection is returned with `:sent` state,
  the given status and response body set to nil.
  """
  defcallback send(status, body, conn)

  @doc """
  Returns the response status if one was set.
  """
  defcallback status(conn)

  @doc """
  Sets the response status and changes the state to `:set`.
  """
  defcallback status(status, conn)

  @doc """
  Returns the response body if one was set.
  """
  defcallback resp_body(conn)

  @doc """
  Sets the response body and changes the state to `:set`.
  """
  defcallback resp_body(body, conn)

  @doc """
  Sets a response to the given status and body. The
  response will only be sent when `send` is called.

  After calling this function, the state changes to `:set`,
  both `status` and `resp_body` are set.
  """
  defcallback resp(status, body, conn)

  @doc """
  A shortcut to `conn.send(conn.status, conn.resp_body)`.
  """
  defcallback send(conn)

  @doc """
  Sends the file at the given path. It is expected that the
  given path exists and it points to a regular file. The
  file is sent straight away.
  """
  defcallback sendfile(path, conn)

  @doc """
  Returns the response state. It can be:

  * `:unset` - the response was not configured yet
  * `:set` - the response was configured via `conn.resp`
  * `:chunked` - the response is being sent in chunks
  * `:sent` - the response was sent

  """
  defcallback state(conn)

  @doc """
  Returns the response headers as `Binary.Dict`.
  """
  defcallback resp_headers(conn)

  @doc """
  Sets a response header, overriding any previous value.
  Both `key` and `value` are converted to binary.
  """
  defcallback set_resp_header(key, value, conn)

  @doc """
  Deletes a response header.
  """
  defcallback delete_resp_header(key, conn)

  ## Cookies

  @doc """
  Returns the cookies sent in the request as a `Binary.Dict`.
  Cookies need to be explicitly fetched with `conn.fetch(:cookies)`
  before using this function.
  """
  defcallback req_cookies(conn)

  @doc """
  Returns a Binary.Dict with cookies. Cookies need to be explicitly
  fetched with `conn.fetch(:cookies)` before using this function.
  """
  defcallback cookies(conn)

  @doc """
  Returns the response cookies as a list of three element tuples
  containing the key, value and given options.
  """
  defcallback resp_cookies(conn)

  @doc """
  Sets a cookie with given key and value and the given options.

  ## Options

  * `max_age` - The cookie max-age in seconds. In order to support
    older IE versions, setting `max_age` also sets the Expires, which
    the developer may customize by passing `local_time`;

  * `secure` - Marks the cookie as secure;

  * `domain` - The domain to which the cookie applies;

  * `path` - The path to which the cookie applies;

  * `http_only` - If the cookie is sent only via http. Default to true;

  """
  defcallback set_cookie(key, value, opts // [], conn)

  @doc """
  Deletes a cookie. The same options given when setting the cookie
  must be given on delete to ensure the browser will pick them up.
  """
  defcallback delete_cookie(key, opts // [], conn)

  ## Misc

  @doc """
  Responsible for fetching and caching aspects of the response.
  The "fetchable" aspects are: headers, params, cookies and session.
  """
  defcallback fetch(aspect, conn)

  @doc """
  Returns a keywords list with assigns set so far.
  """
  defcallback assigns(conn)

  @doc """
  Sets a new assign with the given key and value.
  """
  defcallback assign(key, value, conn)
end