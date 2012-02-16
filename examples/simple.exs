# Run this app from root with:
#
#   elixir -pa ebin examples/simple.exs

defmodule MyApp do
  use Dynamo::Dispatcher

  get "/foo/bar" do
    request.ok("Hello World")
  end
end

Code.prepend_path("deps/misultin/ebin")
Dynamo.run MyApp