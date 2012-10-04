defmodule Dynamo.View.Renderer do
  @moduledoc false
  @slots 1_000_000
  @max_attempts 1_000

  use GenServer.Behaviour
  alias Dynamo.View.Template, as: Template

  @doc """
  Starts the `Dynamo.View.Renderer` server.
  Usually called internally by Dynamo.
  """
  def start_link do
    :gen_server.start({ :local, __MODULE__ }, __MODULE__, [], [])
  end

  @doc """
  Stops the `Dynamo.View.Renderer` server.
  """
  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  @doc """
  Clear compiled templates cache.
  """
  def clear do
    :gen_server.cast(__MODULE__, :clear)
  end

  @doc """
  This function is responsible for rendering the templates.
  It supports both pre-compiled and on demand compilation.

  The on demand mode needs to be explicitly enabled
  by calling start_link/0.
  """
  def render(Template[ref: { mod, fun }, handler: handler], locals, assigns, _prelude) do
    handler.render(mod, fun, locals, assigns)
  end

  def render(Template[handler: handler] = template, locals, assigns, prelude) do
    module =
      case get_module(template) do
        { :ok, mod } ->
          mod
        { :reserved, mod } ->
          compile(mod, template, Keyword.keys(locals), prelude)
        :unavailable ->
          raise_too_busy(template)
      end

    handler.render(module, :render, locals, assigns)
  end

  ## Helpers

  defp get_module(Template[identifier: identifier, updated_at: updated_at]) do
    :gen_server.call(__MODULE__, { :get_module, identifier, updated_at })
  end

  defp put_module(module, Template[identifier: identifier, updated_at: updated_at]) do
    :gen_server.cast(__MODULE__, { :put_module, module, identifier, updated_at })
  end

  defp compile(module, template, locals, prelude) do
    Template[handler: handler, identifier: identifier] = template
    { args, source } = handler.compile(template, locals)

    contents = quote do
      unquote(prelude.())
      @file unquote(identifier)
      def render(unquote_splicing(args)) do
        unquote(source)
      end
    end

    Module.create(module, contents, __ENV__)
    put_module(module, template)
    module
  end

  defp raise_too_busy(Template[identifier: identifier]) do
    raise "Compiling template #{inspect identifier} exceeded the max number of attempts #{@max_attemps}. What gives?"
  end

  ## Callbacks

  @doc false
  def init(args) do
    { :ok, Binary.Dict.new(args) }
  end

  @doc false
  def handle_call({ :get_module, identifier, updated_at }, _from, dict) do
    case Dict.get(dict, identifier) do
      { module, cached } when updated_at > cached ->
        spawn fn -> purge_module(module) end
        { :reply, generate_suggestion(0), Dict.delete(dict, identifier) }
      { module, _ } ->
        { :reply, { :ok, module }, dict }
      nil ->
        { :reply, generate_suggestion(0), dict }
    end
  end

  def handle_call(:stop, _from, dict) do
    { :stop, :normal, :ok, dict }
  end

  def handle_call(_arg, _from, _dict) do
    super
  end

  def handle_cast(:clear, dict) do
    spawn fn ->
      Enum.each dict, fn({ _, { module, _ } }) ->
        purge_module(module)
      end
    end
    { :noreply, Binary.Dict.new }
  end

  def handle_cast({ :put_module, module, identifier, updated_at }, dict) do
    { :noreply, Dict.put(dict, identifier, { module, updated_at }) }
  end

  def handle_cast(_arg, _dict) do
    super
  end

  ## Server Helpers

  defp purge_module(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defp generate_suggestion(attempts) when attempts < @max_attemps do
    random = :random.uniform(@slots)
    module = Module.concat(Dynamo.View, "Template#{random}")

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