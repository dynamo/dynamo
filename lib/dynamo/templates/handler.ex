defmodule Dynamo.Templates.Handler do
  @moduledoc """
  A module that specifies the handler API and
  small conveniences around it.
  """

  @type  template :: Dynamo.Template.t
  @typep assigns  :: list
  @typep locals   :: list

  use Behaviour

  @doc """
  A template handler must simply implement
  compile, receiving a Dynamo.Template
  record. It must return the arguments and
  a source, which will then be compiled to
  a function.

  A template handler must be necessarily
  named as Dynamo.Templates.EXTHandler where
  EXT is the handler extension.
  """
  defcallback compile(template, locals :: list) :: { args :: list, term }

  @doc """
  Receives a module and function in which the compiled
  template is stored plus the locals and assigns
  to be used on dispatch.
  """
  defcallback render(module, function :: atom, locals, assigns) :: binary

  @doc """
  Get the template handler for the given extension.
  """
  def get!(extension) do
    module = Module.concat(Dynamo.Templates, String.upcase(extension) <> "Handler")
    if Code.ensure_loaded?(module) do
      module
    else
      raise "Could not find handler for extension #{extension}"
    end
  end
end

defmodule Dynamo.Templates.EEXHandler do
  @moduledoc false
  @behaviour Dynamo.Templates.Handler

  def compile(Dynamo.Template[source: source, identifier: identifier], locals) do
    vars   = vars(locals)
    args   = [{ :assigns, 0, nil }|vars]
    match  = match(args)
    source = EEx.compile_string(source, file: identifier)

    { args, quote do
      unquote_splicing(match)
      body = unquote(source)
      { unquote(vars), body }
    end }
  end

  def render(module, function, locals, assigns) do
    apply module, function, [assigns|Keyword.values(locals)]
  end

  defp vars(locals) do
    lc name inlist locals, do: { name, 0, nil }
  end

  defp match(locals) do
    lc var inlist locals, do: { :=, 0, [{ :_, 0, nil }, var] }
  end
end