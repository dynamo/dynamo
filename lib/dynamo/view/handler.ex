defmodule Dynamo.View.Handler do
  @moduledoc """
  A module that specifies the handler API and
  small conveniences around it.
  """

  use Behaviour

  @doc """
  A template handler must simply implement
  compile, receiving a Dynamo.View.Template
  record. It must return the arguments and
  a source, which will then be compiled to
  a function.

  A template handler must be necessarily
  named as Dynamo.View.EXTHandler where
  EXT is the handler extension.
  """
  defcallback compile(template, locals)

  @doc """
  Receives a module and function the compiled
  template is stored plus the locals and assigns
  to be used on dispatch.
  """
  defcallback render(module, function, locals, assigns)

  @doc """
  Get the template handler for the given extension.
  """
  def get!(extension) do
    module = Module.concat(Dynamo.View, upcase(extension) <> "Handler")
    if Code.ensure_loaded?(module) do
      module
    else
      raise "Could not find handler for #{extension}"
    end
  end

  defp upcase(<<h, t :: binary>>) when h in ?a..?z do
    <<h - 32, upcase(t) :: binary>>
  end

  defp upcase(<<h, t :: binary>>) do
    <<h, upcase(t) :: binary>>
  end

  defp upcase(<<>>) do
    <<>>
  end
end

defmodule Dynamo.View.EEXHandler do
  @moduledoc false
  @behaviour Dynamo.View.Handler

  def compile(Dynamo.View.Template[source: source, identifier: identifier], locals) do
    locals = [:assigns|locals]
    match  = match(locals)
    source = EEx.compile_string(source, file: identifier)

    { args(locals), quote do
      __block__ unquote(match)
      unquote(source)
    end }
  end

  def render(module, function, locals, assigns) do
    apply module, function, [assigns|Keyword.values(locals)]
  end

  defp args(locals) do
    lc name inlist locals, do: { name, 0, nil }
  end

  defp match(locals) do
    lc name inlist locals, do: { :=, 0, [{ :_, 0, nil }, { name, 0 , nil }] }
  end
end