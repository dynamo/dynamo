defmodule Dynamo.Filters.MethodOverride do
  @moduledoc """
  A filter to overwrite "POST" method with the one defined in _method parameter.
  """
  def service(conn, fun) do
    if conn.method == "POST" do
      conn = conn.fetch(:params)
      case Dict.get(conn.params, :_method) do
        "DELETE" -> fun.(conn.method("DELETE"))
        "PUT"    -> fun.(conn.method("PUT"))
        "PATCH"  -> fun.(conn.method("PATCH"))
        _        -> fun.(conn)
      end
    else
      fun.(conn)
    end
  end
end
