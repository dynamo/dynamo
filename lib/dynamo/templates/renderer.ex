defmodule Dynamo.Templates.Renderer do
  @moduledoc false
  @slots 1_000_000
  @max_attempts 1_000

  use GenServer.Behaviour
  alias Dynamo.Template, as: Template

  @doc """
  Starts the `Dynamo.Templates.Renderer` server.
  Usually called internally by Dynamo.
  """
  def start_link(name) do
    :gen_server.start({ :local, name }, __MODULE__, name, [])
  end

  @doc """
  Stops the `Dynamo.Templates.Renderer` server.
  """
  def stop(name) do
    :gen_server.call(name, :stop)
  end

  @doc """
  Clear compiled templates cache.
  """
  def clear(name) do
    :gen_server.cast(name, :clear)
  end

  @doc """
  This function is responsible for rendering the templates.
  It supports both pre-compiled and on demand compilation.

  The on demand mode needs to be explicitly enabled
  by calling `start_link/0`.
  """
  def render(_name, %Template{ref: { mod, fun }, handler: handler}, locals, assigns, _prelude) do
    handler.render(mod, fun, locals, assigns)
  end

  def render(name, %Template{handler: handler} = template, locals, assigns, prelude) do
    module =
      case get_module(name, template) do
        { :ok, mod } ->
          mod
        { :reserved, mod } ->
          compile(name, mod, template, Keyword.keys(locals), prelude)
        :unavailable ->
          raise_too_busy(template)
      end

    handler.render(module, :dynamo_template_0, locals, assigns)
  end

  ## Helpers

  defp get_module(name, %Template{identifier: identifier, updated_at: updated_at}) do
    :gen_server.call(name, { :get_module, identifier, updated_at })
  end

  defp put_module(name, module, %Template{identifier: identifier, updated_at: updated_at}) do
    :gen_server.cast(name, { :put_module, module, identifier, updated_at })
  end

  defp compile(name, module, template, locals, prelude) do
    %Template{handler: handler, identifier: identifier, finder: finder} = template
    source = Dynamo.Templates.Finder.source(finder, template)
    { args, source } = handler.compile(template, source, locals)

    contents = quote do
      unquote(prelude.())
      @file unquote(identifier)
      def dynamo_template_0(unquote_splicing(args)) do
        unquote(source)
      end
    end

    Module.create(module, contents, file: identifier)
    put_module(name, module, template)
    module
  end

  defp raise_too_busy(%Template{identifier: identifier}) do
    raise "Compiling template #{inspect identifier} exceeded the max number of attempts #{@max_attempts}. What gives?"
  end

  ## Backend

  @doc false
  def init(name) do
    { :ok, { name, Binary.Dict.new() } }
  end

  @doc false
  def handle_call({ :get_module, identifier, updated_at }, _from, { name, dict }) do
    case Binary.Dict.get(dict, identifier) do
      { module, cached } when updated_at > cached ->
        spawn fn -> purge_module(module) end
        { :reply, generate_suggestion(name, 0), { name, Binary.Dict.delete(dict, identifier) } }
      { module, _ } ->
        { :reply, { :ok, module }, { name, dict } }
      nil ->
        { :reply, generate_suggestion(name, 0), { name, dict } }
    end
  end

  def handle_call(:stop, _from, config) do
    { :stop, :normal, :ok, config }
  end

  def handle_call(arg, from, config) do
    super(arg, from, config)
  end

  def handle_cast(:clear, { name, dict }) do
    spawn fn ->
      Enum.each dict, fn({ _, { module, _ } }) ->
        purge_module(module)
      end
    end
    { :noreply, { name, Binary.Dict.new } }
  end

  def handle_cast({ :put_module, module, identifier, updated_at }, { name, dict }) do
    { :noreply, { name, Binary.Dict.put(dict, identifier, { module, updated_at }) } }
  end

  def handle_cast(arg, config) do
    super(arg, config)
  end

  ## Server Helpers

  defp purge_module(module) do
    :code.delete(module)
    :code.purge(module)
  end

  defp generate_suggestion(name, attempts) when attempts < @max_attempts do
    random = :random.uniform(@slots)
    module = Module.concat(name, "T#{random}")

    if :code.is_loaded(module) do
      generate_suggestion(attempts + 1)
    else
      { :reserved, module }
    end
  end

  defp generate_suggestion(_) do
    :unavailable
  end
end