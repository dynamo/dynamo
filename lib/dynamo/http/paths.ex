defmodule Dynamo.HTTP.Paths do
  @moduledoc false

  @doc """
  Generates functions related to paths. It expects a connection
  record macro with fields:

  * path_info_segments with the current path segments as default;
  * script_name_segments with an empty list as default;

  """
  defmacro __using__(_) do
    quote location: :keep do
      def path_info_segments(connection(path_info_segments: segments)) do
        segments
      end

      def path_info(connection(path_info_segments: segments)) do
        to_path segments
      end

      def script_name_segments(connection(script_name_segments: segments)) do
        segments
      end

      def script_name(connection(script_name_segments: segments)) do
        to_path segments
      end

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