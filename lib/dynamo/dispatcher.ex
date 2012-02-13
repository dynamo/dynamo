# Dynamo::Dispatcher allows any Elixir module to match
# requests based on the path and take action accordingly.
#
# ## Examples
#
#     defmodule MyApp do
#       use Dynamo::Dispatcher
#
#       get "users/:id" do
#         response.write_head 200, [{ "Content-Type", "application/json" }]
#         response.end JSON.encode(User.find(id))
#       end
#     end
#
defmodule Dynamo::Dispatcher do
  # Hook invoked when Dynamo::App is used.
  # It initializes the app data, registers a
  # compile callback and import Dynamo::DSL macros.
  defmacro __using__(module) do
    Module.add_compile_callback module, __MODULE__
    quote do
      import Dynamo::Dispatcher::DSL
    end
  end

  defmacro __compiling__(_) do
    quote do
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