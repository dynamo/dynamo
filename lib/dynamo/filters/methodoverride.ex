defmodule Dynamo.Filters.MethodOverride do
  @moduledoc """
  A filter to overwrite "POST" method with the one defined in _method parameter.
  """
  def prepare(conn) do
    if conn.method == "POST" do
      conn = conn.fetch(:params)
      case Dict.get(conn.params, :_method) do
        "DELETE" -> conn.method("DELETE")
        "PUT"    -> conn.method("PUT")
        "PATCH"  -> conn.method("PATCH")
        _        -> conn
      end
    else
      conn
    end
  end
end
