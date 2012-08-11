defmodule Dynamo.Cowboy.Request do
  require :cowboy_http_req, as: R

  # TODO: Plan a mechanism that makes this easier
  @request              2
  @path_info_segments   3
  @script_info_segments 4
  @params               5
  @cookies              6

  defmacrop _(req) do
    quote do
      elem(unquote(req), 2)
    end
  end

  @doc """
  Builds a new Dynamo.Cowboy.Request based on
  the original Cowboy request object.
  """
  def new(req) do
    { segments, req } = R.path(req)
    { __MODULE__, req, segments, [], nil, nil }
  end

  @doc """
  Returns the query string as a binary.
  """
  def query_string(req) do
    { query_string, _ } = R.raw_qs _(req)
    query_string
  end

  @doc """
  Returns a Binary.Dict with params retrieved from the query
  string or from post body. The parameters need to be explicitly
  fetched with `request.fetch(:params)` before using this function.
  """
  def params(req) do
    elem(req, @params) || raise Dynamo.Request.UnfetchedError, aspect: :params
  end

  @doc """
  Returns a Binary.Dict with cookies. Cookues need to be explicitly
  fetched with `request.fetch(:cookies)` before using this function.
  """
  def cookies(req) do
    elem(req, @cookies) || raise Dynamo.Request.UnfetchedError, aspect: :cookies
  end

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  def path_info_segments(req) do
    elem(req, @path_info_segments)
  end

  @doc """
  Returns the request path relative to the forwarding endpoint
  as a binary.
  """
  def path_info(req) do
    to_path path_info_segments(req)
  end

  @doc """
  Returns the full path segments, as received by the web server.
  """
  def path_segments(req) do
    { segments, _ } = R.path _(req)
    segments
  end

  @doc """
  Returns the full path as a binary, as received by the web server.
  """
  def path(req) do
    { binary, _ } = R.raw_path _(req)
    binary
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as segments.
  """
  def script_info_segments(req) do
    elem(req, @script_info_segments)
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as binary.
  """
  def script_info(req) do
    to_path script_info_segments(req)
  end

  @doc """
  Returns the HTTP method as an atom.

  ## Examples

      request.method #=> :GET

  """
  def method(req) do
    { verb, _ } = R.method _(req)
    verb
  end

  @doc """
  Returns the HTTP version.
  """
  def version(req) do
    { version, _ } = R.version _(req)
    version
  end

  @doc """
  Responsible for fetching and caching aspects of the response.
  The "fetchable" aspects are: `:params`, `:cookies`.
  """
  def fetch(:params, original) do
    { query_string, req } = R.raw_qs _(original)
    params = Dynamo.Request.QueryParser.parse(query_string)
    { params, req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    original /> setelem(@request, req) /> setelem(@params, params)
  end

  def fetch(:cookies, original) do
    { cookies, req } = R.cookies _(original)
    original /> setelem(@cookies, Binary.Dict.new(cookies))
  end

  ## Dynamo API

  # Mounts the request by setting the new path information to the given
  # *segments*. Both script_info/1 and path_segments/1 are updated.
  # The segments given must be a suffix of the current path segments.
  @doc false
  def forward_to(segments, _target, req) do
    current = path_info_segments(req)
    { prefix, ^segments } = Enum.split current, length(current) - length(segments)
    req = setelem(req, @path_info_segments, segments)
    req = setelem(req, @script_info_segments, script_info_segments(req) ++ prefix)
    req
  end

  ## Helpers

  defp to_path(segments) do
    "/" <> Enum.join(segments, "/")
  end
end