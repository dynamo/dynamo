defmodule Dynamo.Filters.Head do
  @moduledoc """
  A simple filter that converts HEAD requests to GET.
  `conn.method` will report to be :GET and
  `conn.original_method` will return :HEAD.
  """
  def service(conn, fun) do
    if conn.method == :HEAD do
      conn = fun.(conn.method(:GET))
      case conn.state do
        :unset -> conn
        :set   -> conn.resp(conn.status, "")
        _      -> raise "Streamed from a HEAD request"
      end
    else
      fun.(conn)
    end
  end
end