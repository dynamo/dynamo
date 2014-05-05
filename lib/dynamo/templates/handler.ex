defmodule Dynamo.Templates.Handler do
  @moduledoc """
  A module that specifies the handler API and
  small conveniences around it.

  A template handler is a module that is capable
  to compile Dynamo templates into executable elixir
  code.

  A template handler must be necessarily named as
  Dynamo.Templates.EXTHandler where EXT is the handler
  extension.
  """

  @type  template :: Dynamo.Template.t
  @typep assigns  :: list
  @typep locals   :: list

  use Behaviour

  @doc """
  A template handler must implement compile, receiving a
  Dynamo.Template record. It must return the AST of the
  compiled source with the set of arguments required
  for the proper evaluation of the AST.
  """
  defcallback compile(template, source :: binary, locals :: list) :: { args :: list, term }

  @doc """
  Receives a module and function in which the compiled
  template is stored plus the locals and assigns to be
  used on rendering.
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

  def compile(%Dynamo.Template{identifier: identifier}, source, locals) do
    vars   = vars(locals)
    args   = [{ :assigns, [], nil }|vars]
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
    for name <- locals, do: { name, [], nil }
  end

  defp match(locals) do
    for var <- locals, do: { :=, [], [{ :_, [], nil }, var] }
  end
end
