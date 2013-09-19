defmodule Dynamo.Router.Filters do
  @moduledoc """
  This module is responsible for providing filters to a router or a Dynamo.
  A filter is a module that is invoked before, during or after a service match.

  While hooks are executed only if a route match, filters are always
  executed. In general, filters are a more low-level mechanism, with
  less conveniences compared to hooks.

  There is also a difference regarding ordering. While filters are invoked
  in the order they are declared, regardless of their behaviour, hooks
  always execute `prepare` hooks first, followed by the `finalize` ones.

  ## Usage

      defmodule MyDynamo do
        use Dynamo.Router
        filter Dynamo.Static.new("/public", :myapp)
      end

  ## Behaviours

  A filter must implement one of the three functions:

  * `prepare/1`  - the filter will be executed before invoking service
  * `service/2`  - the filter will be executed with the service function as argument
  * `finalize/1` - the filter will be executed after invoking the service

  ## Examples

  A filter that adds a Chrome Frame header to the response:

    defmodule ChromeFrameFilter do
      def prepare(conn) do
        conn.put_resp_header("x-ua-compatible", "chrome=1")
      end
    end

  Notice the filter receives a `conn` as argument and must return an
  updated `conn`. A finalize filter works similarly.

  A service filter receives and must return a `conn`, but it also 
  receives a function which should be invoked in order to continue
  the request. Here is a filter that sets the content type to json
  and converts the response body to valid json:

      defmodule JSONFilter do
        def service(conn, fun) do
          conn = conn.put_resp_header("content-type", "application/json")
          conn = fun.(conn)
          conn.resp_body(to_json(conn.resp_body))
        end
  
        def to_json(data), do: ...
      end

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @dynamo_filters []
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__), only: [filter: 1, prepend_filter: 1, delete_filter: 1]
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    module  = env.module
    filters = Module.get_attribute(module, :dynamo_filters)
    escaped = Macro.escape(Enum.reverse(filters))

    code = quote(do: super(conn))
    code = Enum.reduce(filters, code, &compile_filter(&1, &2))

    quote location: :keep do
      defoverridable [service: 1]
      def __filters__(), do: unquote(escaped)
      def service(conn), do: unquote(code)
    end
  end

  @doc """
  Adds a filter to the filter chain.
  """
  defmacro filter(spec) do
    quote do
      @dynamo_filters [unquote(spec)|@dynamo_filters]
    end
  end

  @doc """
  Prepends a filter to the filter chain.
  """
  defmacro prepend_filter(spec) do
    quote do
      @dynamo_filters @dynamo_filters ++ [unquote(spec)]
    end
  end

  @doc """
  Deletes a filter from the filter chain.
  """
  defmacro delete_filter(atom) do
    quote do
      atom = unquote(atom)
      @dynamo_filters Enum.filter(@dynamo_filters, fn(f) -> not unquote(__MODULE__).match?(atom, f) end)
    end
  end

  @doc """
  Matches a filter against the other
  """
  def match?(atom, filter) when is_atom(atom) and is_tuple(filter) do
    elem(filter, 0) == atom
  end

  def match?(atom, atom) when is_atom(atom) do
    true
  end

  def match?(_, _) do
    false
  end

  ## Helpers

  defp compile_filter(filter, acc) do
    case Code.ensure_compiled(extract_module(filter)) do
      { :module, _ } ->
        escaped = Macro.escape(filter)

        cond do
          function_exported?(filter, :service, 2)  ->
            quote do
              unquote(escaped).service(conn, fn(conn) -> unquote(acc) end)
            end
          function_exported?(filter, :prepare, 1)  ->
            quote do
              conn = unquote(escaped).prepare(conn)
              unquote(acc)
            end
          function_exported?(filter, :finalize, 1)  ->
            quote do
              unquote(escaped).finalize(unquote(acc))
            end
          true ->
            raise "filter #{inspect filter} does not implement any of the required functions"
        end
      { :error, _ } ->
        raise "could not find #{inspect filter} to be used as filter"
    end
  end

  defp extract_module(mod) when is_atom(mod) do
    mod
  end

  defp extract_module(tuple) when is_tuple(tuple) and is_atom(elem(tuple, 0)) do
    elem(tuple, 0)
  end
end
