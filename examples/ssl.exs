# Run this app from Dynamo root with:
#
#     mix run -r examples/ssl.exs --no-halt
#
defmodule SSLSample do
  use Dynamo
  use Dynamo.Router

  config :server, port: 3030

  # See other SSL options here:
  # http://www.erlang.org/doc/man/ssl.html
  config :ssl,
    port: 3031,
    keyfile: Path.expand("../ssl/key.pem", __FILE__),
    certfile: Path.expand("../ssl/cert.pem", __FILE__),
    password: "cowboy"

  get "/" do
    conn.send(200, "Hello world with #{conn.scheme}")
  end
end

SSLSample.start_link
SSLSample.run
