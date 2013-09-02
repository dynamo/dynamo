defmodule Dynamo.Filters.Static do
  @moduledoc """
  A Dynamo filter capable of serving static assets.

  It must be initialized passing the path to watch
  for assets and path to the assets from the Dynamo
  root.

  If an assets cannot be found, it simply forwards
  the request to the underlying service.

  ## Examples

  This filter can be used in custom routers as follow:

      defmodule MyDynamo do
        use Dynamo.Router
        filter Dynamo.Filters.Static.new("/public", "priv/static")
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
    segments = lc segment inlist List.wrap(segments), do: URI.decode(segment)

    cond do
      segments in [nil, []] ->
        fun.(conn)
      invalid_path?(segments) ->
        conn.send(400, "")
      true ->
        path = Path.join([conn.main.root, root|segments])
        if File.regular?(path) do
          mimes = :mimetypes.filename(path)
          conn
            .put_resp_header("content-type", hd(mimes))
            .put_resp_header("cache-control", "public, max-age=31536000")
            .sendfile(200, path)
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

  defp invalid_path?([h|_]) when h in [".", "..", ""], do: true
  defp invalid_path?([h|t]) do
    case :binary.match(h, ["/", "\\", ":"]) do
      { _, _ } -> true
      :nomatch -> invalid_path?(t)
    end
  end
  defp invalid_path?([]), do: false
end