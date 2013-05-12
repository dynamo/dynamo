defmodule Mix.Tasks.Compile.Dynamo do
  use Mix.Task

  @hidden true
  @shortdoc "Compile Dynamo source files"
  @recursive true
  @manifest ".compile.dynamo"

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
    compile_exts = project[:elixirc_exts]
    watch_exts   = project[:elixirc_watch_exts]
    source_paths = dynamo[:source_paths]
    templates    = extract_templates(dynamo[:templates_paths])

    # Source files + Mix setup + Dynamo config + Templates
    to_watch = Mix.Utils.extract_files(source_paths, watch_exts)
    to_watch = Mix.Project.config_files ++ to_watch
    to_watch = [Path.join(compile_path, "#{mod}.beam")|to_watch]
    to_watch = to_watch ++ Enum.map(templates, template_mtime(&1))

    manifest = Path.join(compile_path, @manifest)
    stale = Mix.Utils.extract_stale(to_watch, [manifest])

    if opts[:force] or stale != [] do
      if elixir_opts = project[:elixirc_options] do
        Code.compiler_options(elixir_opts)
      end

      to_compile = Mix.Utils.extract_files(source_paths, compile_exts)
      File.mkdir_p!(compile_path)
      Code.delete_path compile_path

      { _current, to_remove } =
        Mix.Utils.manifest manifest, fn ->
          compiled = compile_files to_compile, compile_path, root
          lc { mod, _ } inlist compiled, do: atom_to_binary(mod)
        end

      compile_templates mod, dynamo[:compiled_templates], templates, compile_path

      lc f inlist to_remove, do: File.rm(Path.join(compile_path, f) <> ".beam")
      Code.prepend_path compile_path

      :ok
    else
      acc
    end
  end

  defp extract_templates(paths) do
    lc path inlist paths,
       not Dynamo.Templates.Finder.requires_precompilation?(path),
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
    File.write! Path.join(compile_path, "#{name}.beam"), binary
    Mix.shell.info "Generated #{inspect name}"
  end
end
