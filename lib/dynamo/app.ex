defmodule Dynamo.App do
  @moduledoc false

  use Application.Behaviour

  @doc """
  Manually start the application when such is
  required at compilation time.
  """
  def start do
    :application.ensure_all_started(:dynamo)
  end

  @doc """
  Application module callback. Starts Dynamo's supervisor.
  """
  def start(_type, _args) do
    Dynamo.Supervisor.start_link(Dynamo.Supervisor, [])
  end
end
