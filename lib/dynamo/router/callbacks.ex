defmodule Dynamo.Router.Callbacks do
  @moduledoc """
  This module is responsible for providing both `prepare/1` and
  `finalize/1` callbacks to routers.

  Such callbacks can be specified using:

  * `:function_name` - the atom denotes the function name to be invoked
    in the current module;

  * `{ mod, function }` - the module and an atom representing the function
    to be invoked;

  * `[do: block]` - a chunk of code to be executed as callback. The block
    has access to the connection as `conn`.

  Callbacks receive the connection as argument and must return the update
  connection (if any change happens).

  Note that callbacks are invoked regardless if there was a match or
  not in the current module.

  ## Examples

      defmodule MyApp do
        use Dynamo.Router

        prepare do
          unless conn.cookies[:user_id] do
            redirect_to conn, "/"
          end
        end

        get ...
      end

  """

  defexception InvalidCallbackError, kind: nil, callback: nil, actual: nil do
    def message(exception) do
      "expected #{exception.kind} callback #{inspect exception.callback} to return " <>
        "connection, but got #{inspect exception.actual}"
    end
  end

  @doc false
  defmacro __using__(_) do
    module = __CALLER__.module

    Enum.each [:__prepare_callbacks, :__finalize_callbacks],
      Module.register_attribute(module, &1, accumulate: true, persist: false)

    quote location: :keep do
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__), only: [prepare: 1, finalize: 1]
    end
  end

  @doc false
  defmacro before_compile(module) do
    prepare  = Module.read_attribute(module, :__prepare_callbacks)
    finalize = Module.read_attribute(module, :__finalize_callbacks)
    start    = quote do: conn
    prepare  = Enum.reduce(prepare, start, compile_prepare(&1, &2))
    finalize = Enum.reduce(finalize, start, compile_finalize(&1, &2))

    quote do
      defoverridable [dispatch: 3]

      def dispatch(method, path, conn) do
        conn = super(method, path, unquote(prepare))
        unquote(finalize)
      end
    end
  end

  @doc """
  Defines a prepare callback that is executed before any route matches.
  """
  defmacro prepare(do: block) do
    quote do
      name   = :"__prepare_callback_#{length(@__prepare_callbacks)}"
      args   = quote do: [var!(conn)]
      guards = quote do: [is_tuple(var!(conn))]
      defp name, args, guards, do: unquote(block)
      @__prepare_callbacks name
    end
  end

  defmacro prepare(spec) do
    quote do
      @__prepare_callbacks unquote(spec)
    end
  end

  @doc """
  Defines an after callback that is executed after dispatch.
  """
  defmacro finalize(do: block) do
    quote do
      name   = :"__finalize_callback_#{length(@__finalize_callbacks)}"
      args   = quote do: [var!(conn)]
      guards = quote do: [is_tuple(var!(conn))]
      defp name, args, guards, do: unquote(block)
      @__finalize_callbacks name
    end
  end

  defmacro finalize(spec) do
    quote do
      @__finalize_callbacks unquote(spec)
    end
  end

  defp compile_prepare(atom, acc) when is_atom(atom) do
    case atom_to_binary(atom) do
      "Elixir-" <> _ ->
        compile_prepare({ atom, :prepare }, acc)
      _ ->
        quote do
          case unquote(atom).(conn) do
            conn when is_tuple(conn) -> unquote(acc)
            actual -> raise unquote(InvalidCallbackError), kind: :prepare, callback: unquote(atom), actual: actual
          end
        end
    end
  end

  defp compile_prepare({ mod, function } = c, acc) when is_atom(mod) or is_atom(function) do
    quote do
      case apply(unquote(mod), unquote(function), [conn]) do
        conn when is_tuple(conn) -> unquote(acc)
        actual -> raise unquote(InvalidCallbackError), kind: :prepare, callback: unquote(c), actual: actual
      end
    end
  end

  defp compile_finalize(atom, acc) when is_atom(atom) do
    case atom_to_binary(atom) do
      "Elixir-" <> _ ->
        compile_finalize({ atom, :finalize }, acc)
      _ ->
        quote do
          case unquote(atom).(conn) do
            conn when is_tuple(conn) -> unquote(acc)
            actual -> raise unquote(InvalidCallbackError), kind: :finalize, callback: unquote(atom), actual: actual
          end
        end
    end
  end

  defp compile_finalize({ mod, function } = c, acc) when is_atom(mod) or is_atom(function) do
    quote do
      case apply(unquote(mod), unquote(function), [conn]) do
            conn when is_tuple(conn) -> unquote(acc)
        actual -> raise unquote(InvalidCallbackError), kind: :finalize, callback: unquote(c), actual: actual
      end
    end
  end
end