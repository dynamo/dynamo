defmodule Dynamo.Cowboy.Request do
  require :cowboy_http_req, as: R

  @doc """
  Builds a new Dynamo.Cowboy.Request based on
  the original Cowboy request object.
  """
  def new(req) do
    { segments, _ } = R.path(req)
    { __MODULE__, req, segments, [] }
  end

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  def path_info_segments(req) do
    elem(req, 3)
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
    elem(req, 4)
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

  ## Dynamo API

  # Mounts the request by setting the new path information to the
  # *segments*. Both script_info/1 and path_segments/1 are updated.
  # The segments given must be a suffix of the current path segments.
  @doc false
  def forward_to(req, segments, _target) do
    current = path_info_segments(req)
    { prefix, ^segments } = Enum.split current, length(current) - length(segments)
    req = setelem(req, 3, segments)
    req = setelem(req, 4, script_info_segments(req) ++ prefix)
    req
  end

  ## Helpers

  defp to_path(segments) do
    "/" <> Enum.join(segments, "/")
  end

  defp _(req) do
    elem(req, 2)
  end
end