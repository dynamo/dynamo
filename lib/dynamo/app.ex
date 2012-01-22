defmodule Dynamo::App do
  # Hook invoked when Dynamo::App is used.
  # It initializes the app data, registers a
  # compile callback and import Dynamo::DSL macros.
  defmacro __using__(module) do
    Module.merge_data module, routes: []
    Module.add_compile_callback module, __MODULE__
    quote { import Dynamo::DSL }
  end

  defmacro __compiling__(module) do
    # Compile routes
    routes = Module.read_data module, :routes
    Dynamo::Router.compile(module, routes)

    # Generate both an service entry points
    quote do
      # Clean up routes
      @routes nil

      def run(options // []) do
        Dynamo.run(__MODULE__, options)
      end

      def service(request, response) do
        { :abs_path, path } = request.get(:uri)
        verb = request.get(:method)

        case recognize_route(verb, path, []) do
        match: { :ok, fun, _ }
          apply __MODULE__, fun, [request, response]
        match: :error
          request.respond(404, [], "Status: 404")
        end
      end
    end
  end
end