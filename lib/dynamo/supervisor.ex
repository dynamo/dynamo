defmodule Dynamo.Supervisor do
  @moduledoc """
  Dynamo's supervisor. It is a simple one for one,
  the local name, `:max_restart` and `:max_seconds`
  can be configured on start.
  """

  use Supervisor.Behaviour

  def start_link(name, opts) do
    :supervisor.start_link({ :local, name }, __MODULE__, opts)
  end

  def init(opts) do
    opts = Keyword.put opts, :strategy, :one_for_one
    supervise([], opts)
  end
end
