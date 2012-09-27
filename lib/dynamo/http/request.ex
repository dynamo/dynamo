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

      def method(connection(method: method)) do
        method
      end

      def original_method(connection(original_method: method)) do
        method
      end

      def method(method, connection(method: original_method) = conn) when is_binary(method) do
        connection(conn,
          method: method,
          original_method: original_method)
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