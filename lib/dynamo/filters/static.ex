defmodule Dynamo.Filters.Static do
  @moduledoc """
  A Dynamo filter capable of serving static assets.

  It must be initialized passing the root option,
  which may be a directory of an atom representing
  an application to use the root directory.

  If an assets cannot be found, it simply forwards
  the request to the underlying application.

  ## Examples

      defmodule MyApp do
        use Dynamo.App

        filter Dynamo.Filters.Static.new("/public", :my_app)
      end

  """

  @doc false
  def new(path, root) do
    { __MODULE__, Dynamo.Router.Utils.split(path), root }
  end

  @doc false
  def service(conn, fun, { __MODULE__, path, root }) do
    segments = subset(path, conn.path_info_segments)
    if segments == nil or invalid_path?(segments) do
      fun.(conn)
    else
      path = File.join([root_path(root)|segments])
      if File.regular?(path) do
        mimes = :mimetypes.filename(path)
        conn.set_resp_header("Content-Type", hd(mimes)).sendfile(path)
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

  defp root_path(root) when is_atom(root) do
    :code.lib_dir(root, :public)
  end

  defp root_path(root) when is_binary(root) do
    root
  end
end