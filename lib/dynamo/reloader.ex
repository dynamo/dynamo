defmodule Dynamo.Reloader do
  @moduledoc """
  This module is responsible for managing code
  reloading used in development environments in
  Dynamo. The reloader works per-process, so
  each process which requires reloading semantics
  must be explicitly enabled with
  `Dynamo.Reloader.enable`.
  """

  use GenServer.Behaviour

  def enable do
    if Process.whereis(__MODULE__) do
      Process.put(:elixir_ensure_compiled, true)
      Process.flag(:error_handler, Dynamo.Reloader.ErrorHandler)
      :ok
    else
      :disabled
    end
  end

  def start_link(paths) do
    { :module, _ } = Code.ensure_loaded(Dynamo.Reloader.ErrorHandler)
    :gen_server.start({ :local, __MODULE__ }, __MODULE__, paths, [])
  end

  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  def load_missing(module) do
    path = Mix.Utils.underscore(module) <> ".ex"
    dirs = :gen_server.call(__MODULE__, :paths)
    dir  = Enum.find dirs, fn(dir) -> File.regular?(File.join(dir, path)) end

    if dir do
      file    = File.join(dir, path)
      tuples  = Code.require_file(file) || []
      modules = lc { mod, _ } inlist tuples, do: mod
      :gen_server.cast(__MODULE__, { :loaded, file, modules })
    end

    :ok
  end

  def conditional_purge do
    :gen_server.call(__MODULE__, :conditional_purge)
  end

  ## Callbacks

  defrecord Config, loaded_modules: [], loaded_files: [], paths: nil,
    updated_at: { { 1970, 1, 1 }, { 0, 0, 0 } }

  @doc false
  def init(paths) do
    { :ok, Config[paths: paths] }
  end

  @doc false
  def handle_call(:paths, _from, Config[paths: paths] = config) do
    { :reply, paths, config }
  end

  def handle_call(:conditional_purge, _from, Config[updated_at: updated_at] = config) do
    last_modified = last_modified(config)

    if last_modified == updated_at do
      { :reply, :ok, config }
    else
      purge_all(config)
      unload_all(config)
      { :reply, :purged, config.loaded_modules([]).loaded_files([]).updated_at(last_modified) }
    end
  end

  def handle_call(:stop, _from, state) do
    { :stop, :normal, :ok, state }
  end

  def handle_call(_arg, _from, _config) do
    super
  end

  @doc false
  def handle_cast({ :loaded, file, modules }, config) do
    { :noreply, config.prepend_loaded_modules(modules).prepend_loaded_files([file]) }
  end

  def handle_cast(_arg, _config) do
    super
  end

  ## Helpers

  defp purge_all(config) do
    Enum.each config.loaded_modules, fn(mod) ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  defp unload_all(config) do
    Code.unload_files config.loaded_files
  end

  defp last_modified(Config[paths: paths, updated_at: updated_at]) do
    Enum.reduce paths, updated_at, fn(path, acc) ->
      Enum.reduce File.wildcard("#{path}/**/*.ex"), acc, last_modified(&1, &2)
    end
  end

  defp last_modified(path, latest) do
    case File.stat(path) do
      { :ok, File.Stat[mtime: mtime] } -> max(latest, mtime)
      { :error, _ } -> latest
    end
  end
end