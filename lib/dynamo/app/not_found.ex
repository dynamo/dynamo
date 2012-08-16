defmodule Dynamo.App.NotFound do
  @moduledoc """
  This module is responsible for providing the not_found
  cascade functionality available in `Dynamo.App`
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      def service(conn) do
        conn = super

        if conn.status == 404 and conn.state == :set do
          not_found(conn)
        else
          conn
        end
      end

      defoverridable [service: 1]
    end
  end
end