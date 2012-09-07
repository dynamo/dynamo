defmodule Dynamo.View.Handler do
  @moduledoc """
  A module that specifies the handler API and
  small conveniences around it.
  """

  use Behaviour

  @doc """
  A template handler must simply implement
  compile, receiving a Dynamo.View.Template
  record.

  A template handler must be necessarily
  named as Dynamo.View.EXTHandler where
  EXT is the handler extension.
  """
  defcallback compile(template)

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

  defp upcase(<<h, t | :binary>>) when h in ?a..?z do
    <<h - 32, upcase(t) | :binary>>
  end

  defp upcase(<<h, t | :binary>>) do
    <<h, upcase(t) | :binary>>
  end

  defp upcase(<<>>) do
    <<>>
  end
end

defmodule Dynamo.View.EEXHandler do
  @behaviour Dynamo.View.Handler

  def compile(Dynamo.View.Template[source: source, identifier: identifier]) do
    EEx.compile_string(source, file: identifier)
  end
end