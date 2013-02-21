# Run this app from Dynamo root with:
#
#     mix run -r examples/hello_world.exs --no-halt
#
defmodule HelloWorld do
  use Dynamo
  use Dynamo.Router

  config :dynamo, compile_on_demand: false
  config :server, port: 3030

  get "/" do
    conn.resp_body("Hello World!")
  end
end

HelloWorld.start_link
HelloWorld.run
