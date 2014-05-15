defmodule Dynamo.Filters.MethodOverride do
  @moduledoc """
  A filter to overwrite "POST" method with the one defined in _method parameter.
  """
  def prepare(conn) do
    if conn.method == "POST" do
      case method_override(conn) do
        "DELETE" -> conn.method("DELETE")
        "PUT"    -> conn.method("PUT")
        "PATCH"  -> conn.method("PATCH")
        _        -> conn
      end
    else
      conn
    end
  end

  defp method_override(conn) do
    conn = conn.fetch([:params, :headers])
    Binary.Dict.get(conn.params, :_method) || Binary.Dict.get(conn.req_headers, "x-http-method-override")
  end
end
