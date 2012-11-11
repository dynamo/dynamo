defmodule Dynamo.Router.Hooks do
  @moduledoc """
  This module is responsible for providing both `prepare/1` and
  `finalize/1` hooks to routers.

  Such hooks can be specified using:

  * `:function_name` - the atom denotes the function name to be invoked
    in the current module;

  * `module_or_tuple` - a module or a tuple where service will be invoked
     passing the connection as argument;

  * `[do: block]` - a chunk of code to be executed as hook. The block
    has access to the connection as `conn`.

  Hooks receive the connection as argument and must return the updated
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

  Notice that, if a prepare hook replies, redirects or anything,
  the stack aborts and the connection is returned.
  """

  defexception InvalidHookError, kind: nil, hook: nil, actual: nil do
    def message(exception) do
      "expected #{exception.kind} hook #{inspect exception.hook} to return " <>
        "a HTTP connection, but got #{inspect exception.actual}"
    end
  end

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      Enum.each [:__prepare_hooks, :__finalize_hooks],
        Module.register_attribute(__MODULE__, &1, accumulate: true, persist: false)

      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(module) do
    prepare  = Module.get_attribute(module, :__prepare_hooks)
    finalize = Module.get_attribute(module, :__finalize_hooks)

    prepare = Enum.reduce prepare, quote(do: { :ok, conn }), fn(hook, acc) ->
      compile_hook(hook, acc, function(:compile_prepare, 3))
    end

    finalize = Enum.reduce finalize, quote(do: conn), fn(hook, acc) ->
      compile_hook(hook, acc, function(:compile_finalize, 3))
    end

    quote location: :keep do
      @doc false
      def run_prepare_hooks(conn),  do: unquote(prepare)

      @doc false
      def run_finalize_hooks(conn), do: unquote(finalize)
    end
  end

  @doc """
  Defines a prepare hook that is executed before any route matches.
  """
  defmacro prepare(do: block) do
    quote do
      name   = :"__prepare_hook_#{length(@__prepare_hooks)}"
      args   = quote do: [var!(conn)]
      guards = quote do: [is_tuple(var!(conn))]
      defp name, args, guards, do: unquote(Macro.escape block)
      @__prepare_hooks name
    end
  end

  defmacro prepare(spec) do
    quote do
      @__prepare_hooks unquote(spec)
    end
  end

  @doc """
  Defines an after hook that is executed after dispatch.
  """
  defmacro finalize(do: block) do
    quote do
      name   = :"__finalize_hook_#{length(@__finalize_hooks)}"
      args   = quote do: [var!(conn)]
      guards = quote do: [is_tuple(var!(conn))]
      defp name, args, guards, do: unquote(Macro.escape block)
      @__finalize_hooks name
    end
  end

  defmacro finalize(spec) do
    quote do
      @__finalize_hooks unquote(spec)
    end
  end

  defp compile_hook(ref, acc, fun) when is_atom(ref) do
    case atom_to_binary(ref) do
      "Elixir-" <> _ ->
        call = quote(do: unquote(ref).service(conn))
        fun.(call, ref, acc)
      _ ->
        call = quote(do: unquote(ref).(conn))
        fun.(call, ref, acc)
    end
  end

  defp compile_hook(ref, acc, fun) when is_tuple(ref) do
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
          raise unquote(InvalidHookError), kind: :prepare, hook: unquote(ref), actual: actual
      end
    end
  end

  defp compile_finalize(call, ref, acc) do
    quote do
      case unquote(call) do
        conn when is_tuple(conn) -> unquote(acc)
        actual ->
          raise unquote(InvalidHookError), kind: :finalize, hook: unquote(ref), actual: actual
      end
    end
  end
end