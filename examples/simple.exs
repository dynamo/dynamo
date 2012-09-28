# Run this app from root with:
#
#   elixir -pa ebin --no-halt examples/simple.exs

defmodule MyApp do
  use Dynamo.Router
  use Dynamo.App

  get "/foo/bar" do
    conn.resp_body("Hello World!")
  end
end

Code.prepend_path("deps/cowboy/ebin")

Dynamo.start(:prod, __FILE__)
MyApp.start.run port: 3030