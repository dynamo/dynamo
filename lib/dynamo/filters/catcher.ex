defmodule Dynamo.Filters.Catcher do
  @moduledoc """
  A filter that is responsible to catch, debug and
  handle exceptions.
  """
  def service(conn, fun) do
    try do
      fun.(conn)
    catch
      { :dynamo_halt, conn } -> conn
    end
  end
end
