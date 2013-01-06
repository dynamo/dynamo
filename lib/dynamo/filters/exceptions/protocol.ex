defprotocol Dynamo.Exception do
  @moduledoc """
  This protocol specifies how Dynamo should react to specific
  exceptions. particularly, the status code to be used when an
  exception happens.
  """

  @only [Record, Any]

  @doc """
  Returns an integer as the exception status code.
  """
  def status(exception)
end

defimpl Dynamo.Exception, for: Any do
  def status(_), do: 500
end
