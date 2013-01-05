defmodule Dynamo.Helpers.Escaping do
  @moduledoc """
  Conveniences for escaping html and
  other common outputs in views.
  """

  @doc """
  Escapes HTML.
  """
  def h(binary) do
    bc <<code>> inbits to_binary(binary) do
      << case code do
           ?& -> "&amp;"
           ?< -> "&lt;"
           ?> -> "&gt;"
           ?" -> "&quot;"
           _  -> <<code>>
         end :: binary >>
    end
  end
end
