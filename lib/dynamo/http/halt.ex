defmodule Dynamo.HTTP.Halt do
  @moduledoc """
  Conveniences to halt a request.
  A filter needs to be in place to
  handle the halted request.
  """

  @doc """
  Halts the request.

  ## Examples

      unless conn.session[:user_info], do: halt! conn.status(401)

  """
  def halt!(conn) do
    throw { :halt!, conn }
  end
end