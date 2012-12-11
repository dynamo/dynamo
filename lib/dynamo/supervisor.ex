defmodule Dynamo.Supervisor do
  @moduledoc """
  Dynamo's base supervisor. Each application starts its
  own copy of the `Dynamo.Supervisor` and attach children
  to it throught the boot process.

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
  Add a child to the given Dynamo supervisor.
  It accepts the same options as `Supervisor.Behaviour.worker/3`.

  ## Examples

      Dynamo.Supervisor.start_child MyDynamo, Worker, []

  """
  def start_child(dynamo, name, args, opts // []) do
    id = if dynamo == __MODULE__, do: dynamo, else: dynamo.supervisor
    :supervisor.start_child(id, worker(name, args, opts))
  end

  @doc false
  def init(opts) do
    opts = Keyword.put opts, :strategy, :one_for_one
    supervise([], opts)
  end
end
