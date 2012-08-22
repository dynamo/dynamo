defmodule Dynamo.App.NotFound do
  @moduledoc """
  This module is responsible for providing the not_found
  cascade functionality available in `Dynamo.App`
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @before_compile unquote(__MODULE__)

      @doc false
      def not_found(conn) do
        conn.resp(404, "Not found")
      end

      defoverridable [not_found: 1]
    end
  end

  @doc false
  defmacro before_compile(_) do
    quote location: :keep do
      defoverridable [service: 1]

      def service(conn) do
        conn = super

        if conn.status == 404 and conn.state == :set do
          not_found(conn)
        else
          conn
        end
      end
    end
  end
end