defmodule Dynamo.Static do
  @moduledoc false

  use GenServer.Behaviour

  @doc """
  Looks up a static asset in the given Dynamo. If the
  static asset exists, it returns the asset path appended
  with a query string containing the last modification
  time of the asset.
  """
  def lookup(dynamo, path) do
    path = normalize_path(path)
    { ets, server } = dynamo.static_cache
    case :ets.lookup(ets, path) do
      [{ ^path, result }] -> result
      _ -> :gen_server.call(server, { :lookup, path })
    end
  end

  defp normalize_path("/" <> bin), do: bin
  defp normalize_path(bin),        do: bin

  ## Backend

  @doc false
  def start_link(dynamo) do
    { _ets, server } = dynamo.static_cache
    :gen_server.start({ :local, server }, __MODULE__, dynamo, [])
  end

  @doc false
  def stop(dynamo) do
    { _ets, server } = dynamo.static_cache
    :gen_server.call(server, :stop)
  end

  defmodule Config do
    defstruct [:ets, :route, :root, :cache]
  end

  @doc false
  def init(dynamo) do
    config = dynamo.config[:dynamo]
    root   = Path.expand(config[:static_root], dynamo.root)
    route  = config[:static_route]
    cache  = config[:cache_static]

    { ets, _server } = dynamo.static_cache
    :ets.new(ets, [:set, :protected, :named_table, { :read_concurrency, true }])
    { :ok, %Config{ets: ets, route: route, root: root, cache: cache} }
  end

  @doc false
  def handle_call({ :lookup, path }, _from, %Config{} = config) do
    result = static_path(path, config)
    if config.cache, do: :ets.insert(config.ets, { path, result })
    { :reply, result, config }
  end

  def handle_call(:stop, _from, config) do
    { :stop, :normal, :ok, config }
  end

  def handle_call(msg, from, config) do
    super(msg, from, config)
  end

  ## Server helpers

  defp static_path(path, %Config{} = config) do
    case File.stat(Path.join(config.root, path)) do
      { :ok, File.Stat[mtime: mtime, type: type] } when type != :directory and is_tuple(mtime) ->
        seconds = :calendar.datetime_to_gregorian_seconds(mtime)
        Path.join(config.route, [path, ??, integer_to_list(seconds)])
      _ ->
        Path.join(config.route, path)
    end
  end
end
