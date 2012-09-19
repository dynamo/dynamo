defmodule Dynamo.Router.Fetch do
  @moduledoc """
  A convenience that fetches request data from the connection.

  ## Examples

      defmodule MyRouter do
        use Dynamo.Router
        fetch [:params, :cookies]
      end

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__), only: [fetch: 1]
    end
  end

  @doc false
  defmacro before_compile(module) do
    fetch = Module.get_attribute(module, :__fetch)

    if fetch do
      code = Enum.reduce fetch, quote(do: conn), fn(x, acc) ->
        quote(do: unquote(acc).fetch(unquote(x)))
      end

      quote location: :keep do
        defoverridable [service: 1]
        def service(conn), do: super unquote(code)
      end
    end
  end

  @doc """
  A convenience that fetches an aspect of the connection.
  Expects a list containing one or more of the following
  values: headers, params, cookies and session.
  """
  defmacro fetch(aspects) do
    quote do
      @__fetch unquote(aspects)
    end
  end
end