defmodule Dynamo.Static do
  @moduledoc false

  use GenServer.Behaviour

  @doc """
  Looks up a static asset in the given application.
  If the static asset exists, it returns the asset
  path appended with a query string containing the
  last modification time of the asset.
  """
  def lookup(app, path) do
    path = normalize_path(path)
    { ets, server } = app.static_cache
    case :ets.lookup(ets, path) do
      [{ ^path, result }] -> result
      _ -> :gen_server.call(server, { :lookup, path })
    end
  end

  defp normalize_path("/" <> bin), do: bin
  defp normalize_path(bin),        do: bin

  ## Backend

  @doc false
  def start_link(app) do
    { _ets, server } = app.static_cache
    :gen_server.start({ :local, server }, __MODULE__, app, [])
  end

  @doc false
  def stop(app) do
    { _ets, server } = app.static_cache
    :gen_server.call(server, :stop)
  end

  defrecord Config, [:ets, :route, :root, :cache]

  @doc false
  def init(app) do
    dynamo  = app.config[:dynamo]
    root    = File.expand_path(dynamo[:static_root], app.root)
    route   = dynamo[:static_route]
    cache   = dynamo[:cache_static]

    { ets, _server } = app.static_cache
    :ets.new(ets, [:set, :protected, :named_table, { :read_concurrency, true }])
    { :ok, Config[ets: ets, route: route, root: root, cache: cache] }
  end

  @doc false
  def handle_call({ :lookup, path }, _from, Config[] = config) do
    result = static_path(path, config)
    if config.cache, do: :ets.insert(config.ets, { path, result })
    { :reply, result, config }
  end

  def handle_call(:stop, _from, config) do
    { :stop, :normal, :ok, config }
  end

  def handle_call(_msg, _from, _config) do
    super
  end

  ## Server helpers

  defp static_path(path, Config[] = config) do
    case File.stat(File.join(config.root, path)) do
      { :ok, File.Stat[mtime: mtime, type: type] } when type != :directory and is_tuple(mtime) ->
        seconds = :calendar.datetime_to_gregorian_seconds(mtime)
        File.join(config.route, [path, ??, integer_to_list(seconds)])
      _ ->
        File.join(config.route, path)
    end
  end
end
