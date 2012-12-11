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
                    switches: [force: :boolean, quick: :boolean], aliases: [q: :quick])

    Enum.reduce Mix.project[:dynamos], :noop, fn(dynamo, acc) ->
      if dynamo.config[:dynamo][:compile_on_demand] do
        acc
      else
        do_compile(dynamo, opts, acc)
      end
    end
  end

  defp do_compile(mod, opts, acc) do
    root    = File.cwd!
    project = Mix.project
    dynamo  = mod.config[:dynamo]

    compile_path = project[:compile_path]
    compile_exts = project[:compile_exts]
    watch_exts   = project[:watch_exts]
    source_paths = dynamo[:source_paths]
    templates    = extract_templates(dynamo[:templates_paths])

    # Source files + Mix setup + Dynamo config + Templates
    to_watch = Mix.Utils.extract_files(source_paths, watch_exts)
    to_watch = Mix.Project.sources ++ to_watch
    to_watch = [File.join(compile_path, "#{mod}.beam")|to_watch]
    to_watch = to_watch ++ Enum.map(templates, template_mtime(&1))

    stale = Mix.Utils.extract_stale(to_watch, [compile_path])

    if opts[:force] or stale != [] do
      Mix.Task.run "deps.start"

      if elixir_opts = project[:elixirc_options] do
        Code.compiler_options(elixir_opts)
      end

      to_compile = Mix.Utils.extract_files(source_paths, compile_exts)

      Mix.Utils.preserving_mtime(compile_path, fn ->
        File.mkdir_p!(compile_path)
        Code.delete_path compile_path
        compile_files to_compile, compile_path, root
        compile_templates mod, dynamo[:compiled_templates], templates, compile_path
        Code.prepend_path compile_path
      end)

      :ok
    else
      acc
    end
  end

  defp extract_templates(paths) do
    lc path inlist paths,
       not Dynamo.Templates.Finder.precompiled?(path),
       templates = Dynamo.Templates.Finder.all(path),
       template inlist templates, do: template
  end

  defp template_mtime(Dynamo.Template[key: key, updated_at: updated_at]) do
    { key, updated_at }
  end

  defp compile_files(files, to, root) do
    Kernel.ParallelCompiler.files_to_path files, to, fn(original) ->
      relative = :binary.replace original, root <> "/", ""
      Mix.shell.info "Compiled #{relative}"
      original
    end
  end

  defp compile_templates(mod, name, templates, compile_path) do
    binary = Dynamo.Templates.compile_module(name, templates, [:conn],
               fn -> mod.templates_prelude end)
    File.write! File.join(compile_path, "#{name}.beam"), binary
    Mix.shell.info "Generated #{inspect name}"
  end
end
