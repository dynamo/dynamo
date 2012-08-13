# Run this app from root with:
#
#   elixir -pa ebin --no-halt examples/simple.exs

defmodule MyApp do
  use Dynamo.Router

  get "/foo/bar" do
    conn.resp(200, "Hello World!")
  end
end

Code.prepend_path("deps/cowboy/ebin")
Dynamo.Cowboy.run MyApp