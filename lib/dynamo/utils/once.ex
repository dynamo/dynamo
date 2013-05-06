defmodule Dynamo.Utils.Once do
  @moduledoc """
  A convenience that allows a module to be used
  just once via `use_once`.
  """

  @doc false
  defmacro __using__(_) do
    Module.register_attribute(__CALLER__.module, :__use_once, accumulate: true, persist: false)
    quote do
      import unquote(__MODULE__), only: [use_once: 1]
    end
  end

  @doc """
  Uses the given module in the current target
  just once.
  """
  defmacro use_once(module) do
    target   = __CALLER__.module
    expanded = Macro.expand module, __CALLER__

    unless Enum.member?(Module.get_attribute(target, :__use_once), expanded) do
      Module.put_attribute(target, :__use_once, expanded)

      quote do
        use unquote(module)
      end
    end
  end
end