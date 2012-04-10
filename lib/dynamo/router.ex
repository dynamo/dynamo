defmodule Dynamo.Router do
  @moduledoc """
  Dynamo.Routes brings routing semantics to your module.

  ## Examples

      defmodule MyApp do
        use Dynamo.Router

        get "users/:id" do
          response.write_head 200, [{ "Content-Type", "application/json" }]
          response.end JSON.encode(User.find(id))
        end
      end

  """

  @doc false
  defmacro __using__(module, _) do
    Module.add_compile_callback module, __MODULE__

    quote do
      @dynamo_router true
      import Dynamo.Router.DSL

      @overridable true
      def service(req, res) do
        dispatch(req.method, req.path_segments, req, res)
      end

      @overridable true
      def not_found(_req, res) do
        res.reply(404, [], "")
      end
    end
  end

  @doc false
  defmacro __compiling__(_) do
    quote do
      def dispatch(_, _, request, response) do
        not_found(request, response)
      end
    end
  end
end
