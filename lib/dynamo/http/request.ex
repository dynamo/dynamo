defmodule Dynamo.HTTP.Request do
  @moduledoc false

  @doc """
  Generates functions related to paths. It expects a connection
  record macro with fields:

  * params with nil as default;
  * req_headers with nil as default;

  """
  defmacro __using__(_) do
    quote location: :keep do
      def params(connection(params: nil)) do
        raise Dynamo.HTTP.UnfetchedError, aspect: :params
      end

      def params(connection(params: params)) do
        params
      end

      def req_headers(connection(req_headers: nil)) do
        raise Dynamo.HTTP.UnfetchedError, aspect: :req_headers
      end

      def req_headers(connection(req_headers: req_headers)) do
        req_headers
      end
    end
  end
end