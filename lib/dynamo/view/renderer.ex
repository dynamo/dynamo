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
  This function is responsible for rendering the templates.
  It supports both pre-compiled and on demand compilation.

  The on demand mode needs to be explicitly enabled
  by calling start_link/0.
  """
  def render(Template[ref: { mod, fun }], assigns) do
    apply mod, fun, [assigns]
  end

  def render(template, assigns) do
    module =
      get_cached(template) || compile(template) || raise_too_busy(template)

    module.render(assigns)
  end

  ## Callbacks

  @doc false
  def init(args) do
    { :ok, Binary.Dict.new(args) }
  end

  @doc false
  def handle_call({ :get_cached, identifier, updated_at }, _from, dict) do
    case Dict.get(dict, identifier) do
      { module, cached } when updated_at > cached ->
        spawn fn ->
          :code.purge(module)
          :code.delete(module)
        end

        { :reply, nil, Dict.delete(dict, identifier) }
      { module, _ } ->
        { :reply, module, dict }
      nil ->
        { :reply, nil, dict }
    end
  end

  def handle_call({ :register, identifier, updated_at, compiled }, _from, dict) do
    if module = generate_module(compiled, identifier, 0) do
      { :reply, module, Dict.put(dict, identifier, { module, updated_at }) }
    else
      { :reply, nil, dict }
    end
  end

  def handle_call(:stop, _from, state) do
    { :stop, :normal, :ok, state }
  end

  def handle_call(_arg, _from, _config) do
    super
  end

  ## Helpers

  defp get_cached(Template[identifier: identifier, updated_at: updated_at]) do
    :gen_server.call(__MODULE__, { :get_cached, identifier, updated_at })
  end

  defp compile(Template[handler: handler, identifier: identifier, updated_at: updated_at] = template) do
    compiled = Dynamo.View.Handler.get!(handler).compile(template)
    :gen_server.call(__MODULE__, { :register, identifier, updated_at, compiled })
  end

  defp raise_too_busy(Template[identifier: identifier]) do
    raise "Compiling template #{inspect identifier} exceeded the max number of attempts #{@max_attemps}. What gives?"
  end

  defp generate_module(source, identifier, attempts) when attempts < @max_attemps do
    random = :random.uniform(@slots)
    module = Module.concat(Dynamo.View, "Template#{random}")

    if :code.is_loaded(module) do
      generate_module(source, identifier, attempts + 1)
    else
      source = quote hygiene: false do
        _ = assigns
        unquote(source)
      end

      defmodule module do
        @file identifier
        args = quote hygiene: false, do: [assigns]
        def :render, args, [], do: source
      end

      module
    end
  end

  defp generate_module(_, _, _) do
    nil
  end
end