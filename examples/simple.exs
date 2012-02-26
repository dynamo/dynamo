# Run this app from root with:
#
#   elixir -pa ebin --no-stop examples/simple.exs

defmodule MyApp do
  use Dynamo::Router

  get "/foo/bar" do
    { :ok, req } = :cowboy_http_req.reply(200, [], "Hello World!", request)
    req
  end
end

Code.prepend_path("deps/cowboy/ebin")
Dynamo::Cowboy.run MyApp