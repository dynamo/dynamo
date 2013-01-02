# Run this app from root with:
#
#     mix run -r examples/simple.exs --no-halt
#
defmodule MyDynamo do
  use Dynamo
  use Dynamo.Router

  config :dynamo,
    templates_paths: [],
    compile_on_demand: false

  config :server,
    port: 3030

  get "/foo/bar" do
    conn.resp_body("Hello World!")
  end
end

MyDynamo.start_link
MyDynamo.run