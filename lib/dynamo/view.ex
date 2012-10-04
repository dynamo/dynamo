defmodule Dynamo.View do
  defrecord Template, key: nil, identifier: nil, format: nil,
      handler: nil, source: nil, updated_at: nil, ref: nil do
    @moduledoc """
    The template record is responsible to keep information about
    templates to be rendered. It contains:

    * `:key` - The key used to find the template
    * `:identifier` - An unique identifier for the template, like its filesystem path
    * `:format` - The template format
    * `:handler` - The handler responsible for compiling the template
    * `:source` - The template source code
    * `:updated_at` - The last time the template was updated
    * `:ref` - A reference for already compiled templates
    """
  end

  defexception TemplateNotFound, query: nil, view_paths: nil do
    def message(exception) do
      "Could not find template #{inspect exception.query} in any of view paths: #{inspect exception.view_paths}"
    end
  end

  @doc """
  Finds the given template in any of the views paths.
  """
  def find(query, _view_paths) when is_record(query, Template) do
    query
  end

  def find(query, view_paths) do
    Enum.find_value(view_paths, fn(x) -> x.find(query) end)
  end

  @doc """
  Finds the given template in any of the view paths,
  raises `Dynamo.View.TemplateNotfound` if a template
  cannot be found.
  """
  def find!(query, view_paths) do
    find(query, view_paths) ||
      raise Dynamo.View.TemplateNotFound, query: query, view_paths: view_paths
  end

  @doc """
  Renders the given template with the given assigns.
  """
  def render(template, locals, assigns, prelude) do
    Dynamo.View.Renderer.render(template, locals, assigns, prelude)
  end

  @doc """
  Compiles the given set of `templates` into a module
  given by `name`. It returns the module binary,
  """
  def compile_module(name, templates, locals, prelude) do
    { finders, _ } =
      Enum.map_reduce templates, 0, fn(template, i) ->
        template = template.ref({ name, :"template_#{i}" })
        finder   = quote do
          def find(unquote(template.key)) do
            unquote(Macro.escape(template))
          end
        end
        { finder, i + 1 }
      end

    { templates, _ } =
      Enum.map_reduce templates, 0, fn(template, i) ->
        { args, source } = template.handler.compile(template, locals)
        template =
          quote do
            @file unquote(template.identifier)
            def unquote(:"template_#{i}")(unquote_splicing(args)) do
              unquote(source)
            end
          end
        { template, i + 1 }
      end

    contents =
      quote do
        unquote(prelude.())
        unquote(templates)
        unquote(finders)

        def find(_) do
          nil
        end
      end

    { :module, _, binary, _ } = Module.create(name, contents, __ENV__)
    binary
  end
end