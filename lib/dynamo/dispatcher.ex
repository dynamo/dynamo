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
  defmacro __using__(module, _) do
    Module.add_compile_callback module, __MODULE__

    quote do
      import Dynamo::Dispatcher::DSL
      defforward [service: 2, not_found: 2], to: unquote(__MODULE__)
    end
  end

  def service(module, request, response) do
    { :abs_path, path } = request.get(:uri)
    verb = request.get(:method)
    path = Dynamo::Router.split(path)
    module.dispatch(verb, path, request, response)
  end

  def not_found(_module, request, _response) do
    request.respond(404, [], "Status: 404")
  end

  defmacro __compiling__(_) do
    quote do
      def dispatch(_, _, request, response) do
        not_found(request, response)
      end
    end
  end
end