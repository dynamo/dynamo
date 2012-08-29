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

    Dynamo.start

    unless Dynamo.app do
      Code.require_file Mix.project[:dynamo_app] || "config/app.ex"
    end

    app = Dynamo.app

    if app.config[:dynamo][:compile_on_demand] do
      :noop
    else
      dynamo       = app.config[:dynamo]
      root         = dynamo[:root]
      source_paths = dynamo[:source_paths] ++ dynamo[:view_paths]

      unload_app(app)
      eager_compilation(app, root, source_paths, opts, files)
    end
  end

  defp unload_app(app) do
    Dynamo.app(nil)
    :code.purge(app)
    :code.delete(app)
  end

  defp eager_compilation(app, root, source_paths, opts, files) do
    project      = Mix.project
    compile_path = project[:compile_path]
    compile_exts = project[:compile_exts]
    app_beam     = File.join(compile_path, atom_to_binary(app) <> ".beam")
    app_file     = project[:dynamo_app] || "config/app.ex"

    to_compile = [app_file|extract_files(source_paths, files, [:ex])]
    to_watch   = [app_file|extract_files(source_paths, files, compile_exts)]

    if opts[:force] or Mix.Utils.stale?(to_watch, [compile_path, app_beam]) do
      File.mkdir_p!(compile_path)

      if elixir_opts = project[:elixirc_options] do
        Code.compiler_options(elixir_opts)
      end

      compile_files List.uniq(to_compile), compile_path, root
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

  defp compile_files(files, to, root) do
    Kernel.ParallelCompiler.files_to_path files, to, fn(original) ->
      relative = :binary.replace original, root <> "/", ""
      Mix.shell.info "Compiled #{relative}"
      original
    end
  end
end
