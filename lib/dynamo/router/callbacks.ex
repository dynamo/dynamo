defmodule Dynamo.Router.Callbacks do
  @moduledoc """
  This module is responsible for providing both `prepare/1` and
  `finalize/1` callbacks to routers.

  Such callbacks can be specified using:

  * `:function_name` - the atom denotes the function name to be invoked
    in the current module;

  * `module_or_tuple` - a module or a tuple where service will be invoked
     passing the connection as argument;

  * `[do: block]` - a chunk of code to be executed as callback. The block
    has access to the connection as `conn`.

  Callbacks receive the connection as argument and must return the updated
  connection (if any change happens).

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

  Notice that, if a prepare callbacks replies, redirects or anything,
  the stack aborts and the connection is returned.
  """

  defexception InvalidCallbackError, kind: nil, callback: nil, actual: nil do
    def message(exception) do
      "expected #{exception.kind} callback #{inspect exception.callback} to return " <>
        "a HTTP connection, but got #{inspect exception.actual}"
    end
  end

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      Enum.each [:__prepare_callbacks, :__finalize_callbacks],
        Module.register_attribute(__MODULE__, &1, accumulate: true, persist: false)

      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__), only: [prepare: 1, finalize: 1]
    end
  end

  @doc false
  defmacro before_compile(module) do
    prepare  = Module.get_attribute(module, :__prepare_callbacks)
    finalize = Module.get_attribute(module, :__finalize_callbacks)

    prepare = Enum.reduce prepare, quote(do: { :ok, conn }), fn(callback, acc) ->
      compile_callback(callback, acc, function(:compile_prepare, 3))
    end

    finalize = Enum.reduce finalize, quote(do: conn), fn(callback, acc) ->
      compile_callback(callback, acc, function(:compile_finalize, 3))
    end

    quote location: :keep do
      @doc false
      def run_prepare_callbacks(conn),  do: unquote(prepare)

      @doc false
      def run_finalize_callbacks(conn), do: unquote(finalize)
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
      defp name, args, guards, do: unquote(Macro.escape block)
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
      defp name, args, guards, do: unquote(Macro.escape block)
      @__finalize_callbacks name
    end
  end

  defmacro finalize(spec) do
    quote do
      @__finalize_callbacks unquote(spec)
    end
  end

  defp compile_callback(ref, acc, fun) when is_atom(ref) do
    case atom_to_binary(ref) do
      "Elixir-" <> _ ->
        call = quote(do: unquote(ref).service(conn))
        fun.(call, ref, acc)
      _ ->
        call = quote(do: unquote(ref).(conn))
        fun.(call, ref, acc)
    end
  end

  defp compile_callback(ref, acc, fun) when is_tuple(ref) do
    ref  = Macro.escape(ref)
    call = quote(do: unquote(ref).service(conn))
    fun.(call, ref, acc)
  end

  defp compile_prepare(call, ref, acc) do
    quote do
      case unquote(call) do
        conn when is_tuple(conn) ->
          if conn.state != :unset, do: { :abort, conn }, else: unquote(acc)
        actual ->
          raise unquote(InvalidCallbackError), kind: :prepare, callback: unquote(ref), actual: actual
      end
    end
  end

  defp compile_finalize(call, ref, acc) do
    quote do
      case unquote(call) do
        conn when is_tuple(conn) -> unquote(acc)
        actual ->
          raise unquote(InvalidCallbackError), kind: :finalize, callback: unquote(ref), actual: actual
      end
    end
  end
end