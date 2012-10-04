defmodule Dynamo.Helpers do
  @moduledoc """
  A simple module that aggregates all common views
  functionality in Dynamo.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      import Dynamo.Helpers.ContentFor
      import Dynamo.Helpers.Rendering
    end
  end
end