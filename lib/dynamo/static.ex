defmodule Dynamo.Static do
  @moduledoc """
  A Dynamo app capable of serving static assets.
  It must be initialized passing the root option,
  which may be a directory of an atom representing
  an application to use the root directory.

  This application is usually forwarded to and **sets**
  the response to 404 in case the required file cannot
  be found.

  ## Examples

      defmodule MyApp do
        use Dynamo.Router
        use Dynamo.App

        forward "/public", to: Dynamo.Static.new(root: :my_app)
      end

  """

  @doc false
  def new(opts) do
    { __MODULE__, Keyword.get!(opts, :root) }
  end

  @doc false
  def service(conn, { __MODULE__, root }) do
    segments = conn.path_info_segments
    if invalid_path?(segments) do
      not_found(conn)
    else
      path = File.join([root_path(root)|segments])
      if File.regular?(path) do
        mimes = :mimetypes.filename(path)
        conn.set_resp_header("Content-Type", hd(mimes)).sendfile(path)
      else
        not_found(conn)
      end
    end
  end

  defp root_path(root) when is_atom(root) do
    :code.priv_dir(root)
  end

  defp root_path(root) when is_binary(root) do
    root
  end

  defp invalid_path?(segments) do
    Enum.any?(segments, &1 in [".", ".."])
  end

  defp not_found(conn) do
    conn.resp(404, "not found")
  end
end