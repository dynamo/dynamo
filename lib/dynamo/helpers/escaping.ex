defmodule Dynamo.Helpers.Escaping do
  @moduledoc """
  Conveniences for escaping html and
  other common outputs in views.
  """

  @doc """
  Escapes HTML.
  """
  def h(string) do
    bc <<code>> inbits to_string(string) do
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
