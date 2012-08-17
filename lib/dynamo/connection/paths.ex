defmodule Dynamo.Connection.Paths do
  @moduledoc false

  @doc """
  Generates functions related to paths. It expects a connection
  record macro with fields:

  * path_info_segments with the current path segments as default;
  * script_name_segments with an empty list as default;

  """
  defmacro __using__(_) do
    quote location: :keep do 
      @doc """
      Return the path as a list of binaries split on "/".
      If the request was forwarded request, `path_info_segments` returns
      only the segments related to the current forwarded endpoint.
      """
      def path_info_segments(connection(path_info_segments: segments)) do
        segments
      end

      @doc """
      Returns the request path relative to the forwarding endpoint
      as a binary.
      """
      def path_info(connection(path_info_segments: segments)) do
        to_path segments
      end

      @doc """
      As in CGI environment, returns the current forwarded endpoint as segments.
      """
      def script_name_segments(connection(script_name_segments: segments)) do
        segments
      end

      @doc """
      As in CGI environment, returns the current forwarded endpoint as binary.
      """
      def script_name(connection(script_name_segments: segments)) do
        to_path segments
      end

      @doc """
      Mounts the request by setting the new path information to the given
      *segments*. Both script_name/1 and path_segments/1 are updated.
      The segments given must be a suffix of the current path segments.
      """
      def forward_to([], _target, conn) do
        conn
      end

      def forward_to(segments, _target,
          connection(path_info_segments: path, script_name_segments: script) = conn) do
        { prefix, ^segments } = Enum.split path, length(path) - length(segments)

        connection(conn,
          path_info_segments: segments,
          script_name_segments: script ++ prefix
        )
      end

      defp to_path(segments) do
        "/" <> Enum.join(segments, "/")
      end
    end
  end
end