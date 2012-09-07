defmodule Dynamo.Views.EEXHandler do
  # TODO: Define handler behaviour

  def compile(Dynamo.Views.Template[source: source, identifier: identifier]) do
    EEx.compile_string(source, file: identifier)
  end
end