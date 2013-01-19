defmodule Dynamo.Filters.Static do
  @moduledoc """
  A Dynamo filter capable of serving static assets.

  It must be initialized passing the root option,
  which may be a directory of an atom representing
  an application to use the root directory.

  If an assets cannot be found, it simply forwards
  the request to the underlying application.

  ## Examples

  This filter can be used in custom routers as follow:

      defmodule MyDynamo do
        use Dynamo.Router
        filter Dynamo.Filters.Static.new("/public", "priv/app")
      end

  By default, a Dynamo already inserts this filter which
  can be customized via the dynamo configuration. Check
  the `Dynamo` module for more information.
  """

  @doc false
  def new(path, root) do
    { __MODULE__, Dynamo.Router.Utils.split(path), root  }
  end

  @doc false
  def service(conn, fun, { __MODULE__, path, root }) do
    segments = subset(path, conn.path_info_segments)
    if segments == nil or invalid_path?(segments) do
      fun.(conn)
    else
      path = Path.join([conn.app.root, root|segments])
      if File.regular?(path) do
        mimes = :mimetypes.filename(path)
        conn
          .put_resp_header("content-type", hd(mimes))
          .put_resp_header("cache-control", "public, max-age=31536000")
          .sendfile(path)
      else
        fun.(conn)
      end
    end
  end

  defp subset([h|expected], [h|actual]) do
    subset(expected, actual)
  end

  defp subset([], actual) do
    actual
  end

  defp subset(_, _) do
    nil
  end

  defp invalid_path?(segments) do
    Enum.any?(segments, &1 in [".", ".."])
  end
end