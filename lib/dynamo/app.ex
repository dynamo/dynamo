defmodule Dynamo.App do
  @moduledoc false

  use Application.Behaviour

  @doc """
  Manually start the application when such is
  required at compilation time.
  """
  def start do
    :application.start(:crypto)
    :application.start(:mimetypes)

    case :application.start(:dynamo) do
      :ok -> :ok
      { :error, { :already_started, :dynamo } } -> :ok
    end
  end

  @doc """
  Application module callback. Starts Dynamo's supervisor.
  """
  def start(_type, _args) do
    Dynamo.Supervisor.start_link(Dynamo.Supervisor, [])
  end
end
