defmodule Mix.Tasks.Dynamo.App do
  use Mix.Task

  @moduledoc """
  An task that loads and starts up the Dynamo app.
  It is usually used as dependency by other tasks.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)
    mixfile     = Mix.Utils.source(Mix.Project.get!)
    Dynamo.start(Mix.env, File.dirname(mixfile))

    app =
      cond do
        Dynamo.app ->
          Dynamo.app
        opts[:app] ->
          load_app_from_opts(opts[:app])
        lock = Mix.Dynamo.read_lock ->
          load_app_from_lock(lock, opts)
        true ->
          load_app_from_file
      end

    unless opts[:no_start], do: app.start
    app
  end

  defp load_app_from_lock([env, app|_], opts) do
    app = binary_to_atom(app)

    unless env == atom_to_binary(Mix.env) do
      raise Mix.Error, message: %b(the dynamo application #{inspect app} was compiled ) <>
        %b(and locked for environment "#{env}" and you are trying to run it in "#{Mix.env}". ) <>
        %b(You can remove the lock and compiled artifacts with `mix clean`.)
    end

    cond do
      opts[:no_stale] && Mix.Dynamo.stale_app?(app) ->
        load_app_from_file
      not Code.ensure_loaded?(app) ->
        raise Mix.Error, message: "Expected app #{inspect app} in env.lock file to be available, but it is not"
      true ->
        app
    end
  end

  defp load_app_from_opts(app) do
    app = Module.concat([app])
    unless Code.ensure_loaded?(app) do
      raise Mix.Error, message: "Expected given --app #{inspect app} to be available, but it is not"
    end
    app
  end

  defp load_app_from_file do
    Code.require_file Mix.Dynamo.app_file
    app = Dynamo.app || raise "Dynamo.app is not available"

    if app.config[:dynamo][:compile_on_demand] do
      Mix.Task.run "compile"
    else
      Mix.Task.run "loadpaths"
    end

    app
  end
end
