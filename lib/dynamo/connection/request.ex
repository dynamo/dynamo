defmodule Dynamo.Connection.Request do
  @moduledoc false

  @doc """
  Generates functions related to paths. It expects a connection
  record macro with fields:

  * params with nil as default;
  * req_headers with nil as default;

  """
  defmacro __using__(_) do
    quote location: :keep do
      @doc """
      Returns the params retrieved from the query string and the request
      body as a `Binary.Dict`. The parameters need to be explicitly
      fetched with `conn.fetch(:params)` before using this function.
      """
      def params(connection(params: nil)) do
        raise Dynamo.Connection.UnfetchedError, aspect: :params
      end

      def params(connection(params: params)) do
        params
      end

      @doc """
      Returns the request headers as `Binary.Dict`. Note that duplicated
      entries are removed. The headers need to be explicitly fetched with
      `conn.fetch(:headers)` before using this function.
      """
      def req_headers(connection(req_headers: nil)) do
        raise Dynamo.Connection.UnfetchedError, aspect: :req_headers
      end

      def req_headers(connection(req_headers: req_headers)) do
        req_headers
      end
    end
  end
end