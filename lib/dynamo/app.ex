defmodule Dynamo.App do
  @moduledoc false

  use Application.Behaviour

  def start(_type, _args) do
    Dynamo.Supervisor.start_link(Dynamo.Supervisor, [])
  end
end
