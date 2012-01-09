# Run this app with:
#
#   elixir --no-halt -pa ebin -pa deps/misultin/ebin examples/simple.exs

defmodule MyApp do
  use Dynamo::App

  get "/foo/bar" do
    request.ok("Hello World")
  end
end

MyApp.run