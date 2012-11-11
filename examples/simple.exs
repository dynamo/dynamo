# Run this app from root with:
#
#   elixir -pa ebin --no-halt examples/simple.exs
#
Code.prepend_path("deps/ranch/ebin")
Code.prepend_path("deps/cowboy/ebin")

Dynamo.start(:prod)

defmodule MyDynamo do
  use Dynamo.Router
  use Dynamo

  config :dynamo,
    compile_on_demand: false

  get "/foo/bar" do
    conn.resp_body("Hello World!")
  end
end

MyDynamo.start.run port: 3030