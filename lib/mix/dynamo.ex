defmodule Mix.Dynamo do
  @moduledoc false

  @doc """
  Returns the app file location.
  """
  def app_file do
    Mix.project[:dynamo_app] || "config/app.ex"
  end

  @doc """
  Check if the app beam file is stale compared to the app file,
  env files and the mix file.
  """
  def stale_app?(app) do
    mix_file = Mix.Utils.source(Mix.Project.current)
    env_file = "config/environments/#{Dynamo.env}.exs"
    app_beam = File.join(Mix.project[:compile_path], "#{app}.beam")
    Mix.Utils.stale?([app_file(), mix_file, env_file], [app_beam])
  end

  @doc """
  Returns the lock path.
  """
  def lock_path do
    File.join Mix.project[:compile_path], "env.lock"
  end

  @doc """
  Reads the lock file. It returns a list with each line
  in the lock or nil otherwise.
  """
  def read_lock do
    case File.iterator(lock_path) do
      { :ok, iterator } -> Enum.map(iterator, fn(x) -> x end)
      { :error, _ }     -> nil
    end
  end

  @doc """
  Remove the lock (if it exists) and associated artifacts.
  """
  def remove_lock do
    if lock = read_lock do
      File.rm!(lock_path)

      path  = Mix.project[:compile_path]
      files = tl(lock) # Remove env

      Enum.each files, fn(file) ->
        File.rm_rf! File.join(path, "#{file}.beam")
      end
    end
  end

  @doc """
  Takes a lock snapshot.
  """
  def lock_snapshot(fun) do
    remove_lock
    query = File.join Mix.project[:compile_path], "*.beam"

    pre = File.wildcard(query)
    res = fun.()
    pos = File.wildcard(query)

    app = Dynamo.app /> atom_to_binary
    new = Enum.map pos -- pre, &1 /> File.basename /> File.rootname
    new = List.delete(new, app)

    File.open! lock_path, [:write], fn(f) ->
      Enum.each [Mix.env,app|new], IO.puts(f, &1)
    end

    res
  end
end