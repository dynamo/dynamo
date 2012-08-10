defmodule Dynamo.Router.Callbacks do
  @moduledoc """
  This module is responsible for providing both `prepare/1` and
  `finish/1` callbacks to routers.

  Such callbacks can be specified using:

  * `:function_name` - the atom denotes the function name to be invoked
    in the current module;

  * `{ mod, function }` - the module and an atom representing the function
    to be invoked;

  * `[do: block]` - a chunk of code to be executed as callback. The block
    has access to the request and response types via the variables `req`
    and `res`.

  Prepare callbacks must return a tuple `{ req, res }` with the request
  and response, finish callbacks must return the response. Both callbacks
  receive the request and response as arguments.

  Note that callbacks are invoked regardless if there was a match or
  not in the current module.

  ## Examples

      defmodule MyApp do
        use Dynamo.Router

        prepare do
          unless request.cookies[:user_id] do
            redirect_to response, "/"
          end
        end

        get ...
      end

  """

  defexception InvalidPrepareCallbackError, callback: nil, actual: nil do
    def message(exception) do
      "expected prepare callback #{inspect exception.callback} to return " <>
        "{ req, res }, but got #{inspect exception.actual}"
    end
  end

  @doc false
  defmacro __using__(_) do
    module = __CALLER__.module

    Enum.each [:__prepare_callbacks, :__finish_callbacks],
      Module.register_attribute(module, &1, accumulate: true, persist: false)

    quote location: :keep do
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__), only: [prepare: 1, finish: 1]
    end
  end

  @doc false
  defmacro before_compile(module) do
    prepare = Module.read_attribute(module, :__prepare_callbacks)
    start   = quote do: { req, res }
    code    = Enum.reduce(prepare, start, compile_prepare(&1, &2))

    quote do
      defoverridable [dispatch: 4]

      def dispatch(method, path, req, res) do
        { req, res } = unquote(code)
        super(method, path, req, res)
      end
    end
  end

  @doc """
  Defines a prepare callback that is executed before any route matches.
  """
  defmacro prepare(do: block) do
    quote do
      name   = :"__prepare_callback_#{length(@__prepare_callbacks)}"
      args   = quote do: [var!(req), var!(res)]
      guards = quote do: [is_tuple(var!(req)) and is_tuple(var!(res))]
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
  defmacro finish(spec) do
    quote do
      @__finish_callbacks unquote(spec)
    end
  end

  defp compile_prepare(atom, acc) when is_atom(atom) do
    case atom_to_binary(atom) do
      "Elixir-" <> _ ->
        compile_prepare({ atom, :prepare }, acc)
      _ ->
        quote do
          case unquote(atom).(req, res) do
            { req, res } -> unquote(acc)
            actual -> raise unquote(InvalidPrepareCallbackError), callback: unquote(atom), actual: actual
          end
        end
    end
  end

  defp compile_prepare({ mod, function } = c, acc) when is_atom(mod) or is_atom(function) do
    quote do
      case apply(unquote(mod), unquote(function), [req, res]) do
        { req, res } -> unquote(acc)
        actual -> raise unquote(InvalidPrepareCallbackError), callback: unquote(c), actual: actual
      end
    end
  end
end