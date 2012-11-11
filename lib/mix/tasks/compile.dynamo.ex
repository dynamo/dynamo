defmodule Mix.Tasks.Compile.Dynamo do
  use Mix.Task

  @hidden true
  @shortdoc "Compile Dynamo source files"

  @moduledoc """
  A task to compile Dynamo source files.

  This task will compile all dynamos registered in
  your Mix project under the `dynamos` configuration.
  If the current dynamo is set to `:compile_on_demand`
  compilation is actually skipped.

  ## Configuration

  * `:dynamos` - the dynamos registered in this project:

        [dynamos: Foo, Bar]

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
    tmpl_paths   = dynamo[:templates_paths]
    source_paths = dynamo[:source_paths] ++ extract_templates(tmpl_paths)

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
      compile_templates mod, dynamo[:compiled_templates], tmpl_paths, compile_path
      Code.prepend_path compile_path

      :ok
    else
      acc
    end
  end

  defp extract_templates(paths) do
    lc path inlist paths, path = path.to_path, do: path
  end

  defp compile_files(files, to, root) do
    Kernel.ParallelCompiler.files_to_path files, to, fn(original) ->
      relative = :binary.replace original, root <> "/", ""
      Mix.shell.info "Compiled #{relative}"
      original
    end
  end

  defp compile_templates(mod, name, tmpl_paths, compile_path) do
    templates = lc path inlist tmpl_paths,
                   path.eager?,
                   template inlist path.all, do: template

    binary = Dynamo.Templates.compile_module(name, templates, [:conn], fn -> mod.templates_prelude end)
    File.write! File.join(compile_path, "#{name}.beam"), binary

    Mix.shell.info "Generated #{inspect name}"
  end
end
