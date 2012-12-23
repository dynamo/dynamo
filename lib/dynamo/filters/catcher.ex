defmodule Dynamo.Filters.Catcher do
  @moduledoc """
  A filter that is responsible to catch, debug and
  handle exceptions.
  """
  def service(conn, fun) do
    try do
      fun.(conn)
    catch
      { :halt!, conn } -> conn
    end
  end
end
