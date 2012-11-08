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
    { opts, _ } = OptionParser.parse(args,
                    flags: [:force, :quick], aliases: [q: :quick])

    Mix.Task.run "dynamo.start", ["--no-start"]

    Enum.reduce Mix.project[:dynamos], :noop, fn(dynamo, acc) ->
      unless dynamo.config[:dynamo][:compile_on_demand] do
        acc = do_compile(dynamo, opts, acc)
      end
      dynamo.start
      acc
    end
  end

  defp do_compile(mod, opts, acc) do
    root    = File.cwd!
    project = Mix.project
    dynamo  = mod.config[:dynamo]

    compile_path = project[:compile_path]
    compile_exts = project[:compile_exts]
    watch_exts   = project[:watch_exts]
    view_paths   = dynamo[:view_paths]
    source_paths = dynamo[:source_paths] ++ extract_views(view_paths)

    to_compile = Mix.Utils.extract_files(source_paths, compile_exts)
    to_watch   = Mix.Utils.extract_files(source_paths, watch_exts)
    mod_beam   = File.join(compile_path, "#{mod}.beam")
    stale      = Mix.Utils.extract_stale([mod_beam|to_watch], [compile_path])

    if opts[:force] or stale != [] do
      File.mkdir_p!(compile_path)

      if elixir_opts = project[:elixirc_options] do
        Code.compiler_options(elixir_opts)
      end

      Code.delete_path compile_path
      compile_files to_compile, compile_path, root
      compile_views mod, dynamo[:compiled_view_paths], view_paths, compile_path
      Code.prepend_path compile_path

      :ok
    else
      acc
    end
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

  defp compile_views(mod, name, view_paths, compile_path) do
    templates = lc view_path inlist view_paths,
                   view_path.eager?,
                   template inlist view_path.all, do: template

    binary = Dynamo.View.compile_module(name, templates, [:conn], fn -> mod.views end)
    File.write! File.join(compile_path, "#{name}.beam"), binary

    Mix.shell.info "Generated #{inspect name}"
  end
end
