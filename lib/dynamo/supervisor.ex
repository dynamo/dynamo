defmodule Dynamo.Supervisor do
  @moduledoc """
  Dynamo's base supervisor. Each Dynamo starts its own copy of
  the `Dynamo.Supervisor` and attach children to it throughout
  the boot process.

  The local name, `:max_restarts` and `:max_seconds` can
  be configured on `start_link`.
  """

  use Supervisor.Behaviour

  @doc """
  Starts the supervisor. It is automatically started
  when the Dynamo is started.
  """
  def start_link(name, opts) do
    :supervisor.start_link({ :local, name }, __MODULE__, opts)
  end

  @doc """
  Add a child to the given supervisor.
  It accepts the same options as `Supervisor.Behaviour.worker/3`.

  ## Examples

      Dynamo.Supervisor.start_child MyDynamo.supervisor, Worker, []

  """
  def start_child(supervisor, name, args, opts // []) do
    :supervisor.start_child(supervisor, worker(name, args, opts))
  end

  @doc false
  def init(opts) do
    opts = Keyword.put opts, :strategy, :one_for_one
    supervise([], opts)
  end
end
