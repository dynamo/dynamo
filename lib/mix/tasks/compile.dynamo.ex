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

  * `-f`, `--file` - compiles only the given file / pattern;
  * `--force` - forces compilation regardless of mod times;

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, flags: [:force], aliases: [f: :file])
    project = Mix.project

    Dynamo.start

    unless Dynamo.app do
      Code.require_file project[:dynamo_app] || "config/app.ex"
    end

    app = Dynamo.app

    if app.config[:dynamo][:compile_on_demand] do
      :noop
    else
      source_paths = app.config[:dynamo][:source_paths]
      unload_app(app)
      eager_compilation(project, source_paths, opts)
    end
  end

  defp unload_app(app) do
    Dynamo.app(nil)
    :code.purge(app)
    :code.delete(app)
  end

  defp eager_compilation(project, source_paths, opts) do
    compile_path = project[:compile_path] || "ebin"
    app          = project[:dynamo_app] || "config/app.ex"
    to_compile   = [app|extract_files(source_paths, opts[:file])]

    if opts[:force] or Mix.Utils.stale?(to_compile, [compile_path]) do
      File.mkdir_p!(compile_path)

      if elixir_opts = project[:elixirc_options] do
        Code.compiler_options(elixir_opts)
      end

      compile_files List.uniq(to_compile), compile_path
      File.touch(compile_path)
    else
      :noop
    end
  end

  defp extract_files(paths, nil) do
    List.concat(lc path inlist paths do
      File.wildcard("#{path}/**/*.ex")
    end)
  end

  defp extract_files(_, pattern) do
    File.wildcard(pattern)
  end

  defp compile_files(files, to) do
    Kernel.ParallelCompiler.files_to_path files, to, fn(x) ->
      Mix.shell.info "Compiled #{x}"
      x
    end
  end
end
