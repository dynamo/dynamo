# Run this app from Dynamo root with:
#
#     mix run -r examples/layout.exs --no-halt
#
defmodule Layouts do
  use Dynamo
  use Dynamo.Router

  config :dynamo, templates_paths: [Path.expand("../layouts", __FILE__)]
  config :server, port: 3030

  # All we need to do in the router is to assign a layout.
  # Notice that we don't add an extension here, since it
  # picks up the extension from the rendered template.
  prepare do
    conn.assign :layout, "main"
  end

  get "/" do
    render conn, "child.html"
  end
end

Layouts.start_link
Layouts.run
