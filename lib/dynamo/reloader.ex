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

  @doc """
  Enables the reloader in the given process and returns
  `:ok`. In case the reloader server is disabled, it works
  as noop and returns `:error`.
  """
  def enable do
    if Process.whereis(__MODULE__) do
      Process.put(:elixir_ensure_compiled, true)
      Process.flag(:error_handler, Dynamo.Reloader.ErrorHandler)
      :ok
    else
      :error
    end
  end

  @doc """
  Sames as enable/0 but raises if the server is not available.
  """
  def enable! do
    case enable do
      :error -> raise "cannot enable on demand compilation because reloader mode is disabled"
      other  -> other
    end
  end

  @doc """
  Starts the `Dynamo.Reloader` server. Usually called
  internally by Dynamo. The given `paths` must be expanded.
  """
  def append_paths(paths) do
    unless Process.whereis(__MODULE__) do
      { :module, _ } = Code.ensure_loaded(Dynamo.Reloader.ErrorHandler)
      :gen_server.start({ :local, __MODULE__ }, __MODULE__, [], [])
    end

    :gen_server.cast(__MODULE__, { :append_paths, paths })
  end

  @doc """
  Stops the `Dynamo.Reloader` server.
  """
  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  @doc """
  Register a callback that is invoked every time modules are purged.
  """
  def on_purge(fun) when is_function(fun) do
    :gen_server.cast(__MODULE__, { :on_purge, fun })
  end

  @doc """
  Tries to load the missing module. It returns `:ok` if a file
  for the given module could be found and `:error` otherwise.
  Note it does not actually ensure the module was loaded (only
  that the related file was required).
  """
  def load_missing(module) do
    case atom_to_binary(module) do
      "Elixir-" <> _ ->
        path = Mix.Utils.underscore(module) <> ".ex"
        dirs = :gen_server.call(__MODULE__, :paths)
        dir  = Enum.find dirs, fn(dir) -> File.regular?(File.join(dir, path)) end

        if dir do
          file   = File.join(dir, path)
          tuples =
            try do
              Code.require_file(file) || []
            catch
              kind, reason ->
                Code.unload_files [file]
                apply(:erlang, kind, [reason])
            end
          modules = lc { mod, _ } inlist tuples, do: mod
          :gen_server.cast(__MODULE__, { :loaded, file, modules })
          :ok
        else
          :notfound
        end
      _ ->
        :notfound
    end
  end

  @doc """
  Checks if any of the `.ex` files in the registered paths
  were updated and if so, purges all automatically compiled
  and loaded modules, and "unrequire" the relevant files.
  """
  def conditional_purge do
    case :gen_server.call(__MODULE__, :conditional_purge) do
      :ok -> :ok
      { :purged, callbacks } ->
        lc callback inlist callbacks, do: callback.()
        :purged
    end
  end

  ## Callbacks

  defrecord Config, loaded_modules: [], loaded_files: [], paths: nil,
    updated_at: { { 1970, 1, 1 }, { 0, 0, 0 } }, on_purge: []

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
      { :reply, { :purged, Enum.reverse(config.on_purge) },
        config.loaded_modules([]).loaded_files([]).updated_at(last_modified) }
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

  def handle_cast({ :on_purge, fun }, config) do
    { :noreply, config.prepend_on_purge([fun]) }
  end

  def handle_cast({ :append_paths, paths }, config) do
    { :noreply, config.update_paths(&1 ++ paths) }
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