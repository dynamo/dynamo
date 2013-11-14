defmodule Dynamo.Filters.Head do
  @moduledoc """
  A simple filter that converts HEAD requests to GET.
  `conn.method` will report to be "GET" and
  `conn.original_method` will return "HEAD".
  """
  def prepare(conn) do
    if conn.method == "HEAD" do
      conn.method("GET")
    else
      conn
    end
  end
end