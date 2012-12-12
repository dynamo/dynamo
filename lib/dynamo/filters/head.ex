defmodule Dynamo.Filters.Head do
  @moduledoc """
  A simple filter that converts HEAD requests to GET.
  `conn.method` will report to be "GET" and
  `conn.original_method` will return "HEAD".
  """
  def service(conn, fun) do
    if conn.method == "HEAD" do
      fun.(conn.method("GET"))
    else
      fun.(conn)
    end
  end
end