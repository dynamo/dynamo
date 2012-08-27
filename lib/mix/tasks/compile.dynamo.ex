defmodule Mix.Tasks.Compile.Dynamo do
  use Mix.Task

  @hidden true
  @shortdoc "Compile Dynamo source files"

  @moduledoc """
  A task to compile Dynamo source files.

  This tasks first loads the application specified by
  `:dynamo_app` and then based on the environment configuration,
  compiles the Elixir project or just configures the lazy load
  setup.

  ## Configuration

  * `:dynamo_app` - the dynamo app to load

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

    Code.require_file project[:dynamo_app] || "config/app.ex"
    app = Dynamo.app

    if app.config[:dynamo][:compile_on_demand] do
      :noop
    else
      eager_compilation(app, project, opts)
    end
  end

  defp eager_compilation(app, project, opts) do
    source_paths = Keyword.values(app.config[:dynamo][:source_paths])
    compile_path = project[:compile_path]  || "ebin"
    to_compile   = extract_files(source_paths, opts[:file])

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
