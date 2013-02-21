defmodule Dynamo.Filters.Exceptions.Public do
  @moduledoc """
  Display exceptions publicly.
  It simply sets th status code in the conneciton.
  """
  def service(conn) do
    { status, _kind, _value, _stacktrace } = conn.assigns[:exception]
    conn.send(status, "")
  end
end
