defmodule Dynamo::Cowboy::Request do
  require :cowboy_http_req, as: R

  @doc """
  Builds a new Dynamo::Cowboy::Request based on
  the original Cowboy request object.
  """
  def new(req) do
    { segments, _ } = R.path(req)
    { __MODULE__, req, segments, [] }
  end

  @doc """
  Return the path as a list of binaries split on "/".
  If the request is mounted, path_segments returns
  only the segments related to the current mount point.
  """
  def path_segments(req) do
    elem(req, 3)
  end

  @doc """
  Returns the requets path relative to mount point as
  a binary.
  """
  def path(req) do
    "/" <> Enum.join(path_segments(req), "/")
  end

  @doc """
  Returns the full path segments, disregarding the mount point.
  """
  def full_path_segments(req) do
    R.path _(req)
  end

  @doc """
  Returns the full path as a binary, disregarding the mount point.
  """
  def full_path(req) do
    "/" <> Enum.join(path_segments(req), "/")
  end

  @doc """
  As in CGI environment, returns the current mount point.
  """
  def script_info(req) do
    elem(req, 4)
  end

  @doc """
  Mounts the request at the given path *segments* by updating both
  script_info/1 and path_segments/1 to contain the new segments.
  The segments given must be a suffix of the current path segments.
  """
  def mount(segments, req) do
    req = setelem(req, 3, Enum.drop length(segments), path_segments(req))
    req = setelem(req, 4, script_info(req) ++ segments)
    req
  end

  def reply(status, headers, response, req) do
    { :ok, new } = :cowboy_http_req.reply(status, headers, response, _(req))
    _(new, req)
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

  # Returns the original cowboy request object.

  defp _(req) do
    elem(req, 2)
  end

  defp _(value, req) do
    setelem(req, 2, value)
  end
end