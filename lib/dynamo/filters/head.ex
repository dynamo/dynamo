defmodule Dynamo.Filters.Head do
  @moduledoc """
  A simple filter that converts HEAD requests to GET.
  `conn.method` will report to be :GET and
  `conn.original_method` will return :HEAD.
  """
  def service(conn, fun) do
    if conn.method == "HEAD" do
      conn = fun.(conn.method("GET"))
      case conn.state do
        :set -> conn.resp_body("")
        _    -> conn
      end
    else
      fun.(conn)
    end
  end
end