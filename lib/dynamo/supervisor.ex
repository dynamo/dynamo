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
  Add a child to the given supervisor.
  """
  def start_child(app, name, args, opts // []) do
    id = if app == __MODULE__, do: app, else: app.supervisor
    :supervisor.start_child(id, worker(name, args, opts))
  end

  @doc false
  def init(opts) do
    opts = Keyword.put opts, :strategy, :one_for_one
    supervise([], opts)
  end
end
