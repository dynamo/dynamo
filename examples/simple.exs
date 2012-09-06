# Run this app from root with:
#
#   elixir -pa ebin --no-halt examples/simple.exs

defmodule MyApp do
  use Dynamo.Router
  use Dynamo.App

  get "/foo/bar" do
    conn.resp(200, "Hello World!")
  end
end

Code.prepend_path("deps/cowboy/ebin")

Dynamo.start
Dynamo.Cowboy.run MyApp.start, port: 3030

IO.puts "Running MyApp on port 3030"