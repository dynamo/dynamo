defmodule Mix.Tasks.Compile.Dynamo do
  use Mix.Task

  @hidden true
  @shortdoc "Compile Dynamo source files"

  @moduledoc """
  A task to compile Dynamo source files.

  This tasks first loads the application specified by
  `:dynamo_app` and then based on the value of `:compile_on_demand`,
  compiles the Elixir project or just compiles on demand.

  ## Configuration

  * `:dynamo_app` - the dynamo app to load, defaults to "config/app.ex"

        [dynamo_app: "config/other.ex"]

  This task also uses `:compile_path` and `:elixirc_options`
  options shared with other compilation tasks.

  ## Command line options

  * `--force` - forces compilation regardless of mod times;

  """
  def run(args) do
    { opts, files } = OptionParser.parse(args, flags: [:force])
    Dynamo.start(Mix.env)

    # Load the dynamo app but don't start it.
    # We reenable the task so it can be called
    # down the road and the app finally started.
    Mix.Task.run "dynamo.app", ["--no-start"]
    Mix.Task.reenable "dynamo.app"
    app = Dynamo.app

    if app.config[:dynamo][:compile_on_demand] do
      :noop
    else
      do_compile(app, files, opts)
    end
  end

  defp unload_app(app) do
    Dynamo.app(nil)
    :code.purge(app)
    :code.delete(app)
  end

  defp do_compile(app, files, opts) do
    project = Mix.project
    dynamo  = app.config[:dynamo]

    compile_path = project[:compile_path]
    compile_exts = project[:compile_exts]
    view_paths   = dynamo[:view_paths]
    source_paths = dynamo[:source_paths] ++ extract_views(view_paths)

    app_file = project[:dynamo_app] || "config/app.ex"
    env_file = "config/environments/#{Mix.env}.exs"
    app_beam = File.join(compile_path, atom_to_binary(app) <> ".beam")

    to_compile = [app_file|extract_files(source_paths, files, [:ex])]
    to_watch   = [app_file, env_file|extract_files(source_paths, files, compile_exts)]
    targets    = [app_beam, compile_path]

    if opts[:force] or Mix.Utils.stale?(to_watch, targets) do
      unload_app(app)
      File.mkdir_p!(compile_path)

      if elixir_opts = project[:elixirc_options] do
        Code.compiler_options(elixir_opts)
      end

      Mix.Dynamo.lock_snapshot fn ->
        compile_files List.uniq(to_compile), compile_path, dynamo[:root]
        compile_views dynamo[:compiled_view_paths], view_paths, compile_path
      end

      :ok
    else
      :noop
    end
  end

  defp extract_files(paths, [], exts) do
    exts = Enum.join(exts, ",")
    List.concat(lc path inlist paths do
      File.wildcard("#{path}/**/*.{#{exts}}")
    end)
  end

  defp extract_files(paths, files, exts) do
    paths = extract_files(paths, [], exts)
    files = Enum.map files, File.expand_path(&1)
    Enum.filter files, List.member?(paths, &1)
  end

  defp extract_views(view_paths) do
    lc view_path inlist view_paths, path = view_path.to_path, do: path
  end

  defp compile_files(files, to, root) do
    Kernel.ParallelCompiler.files_to_path files, to, fn(original) ->
      relative = :binary.replace original, root <> "/", ""
      Mix.shell.info "Compiled #{relative}"
      original
    end
  end

  defp compile_views(name, view_paths, compile_path) do
    templates = lc view_path inlist view_paths,
                   view_path.eager?,
                   template inlist view_path.all, do: template

    binary = Dynamo.View.compile_module(name, templates)
    File.write! File.join(compile_path, "#{name}.beam"), binary

    Mix.shell.info "Generated #{inspect name}"
  end
end
