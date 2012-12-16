Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Cowboy.SSLTest do
  use ExUnit.Case, async: true

  defmodule App do
    use Dynamo

    def service(conn) do
      conn.send(200, "scheme: #{conn.scheme}")
    end

    config :server, port: 8021

    config :ssl,
      port: 8022,
      password: "cowboy",
      keyfile: File.expand_path("../../../fixtures/ssl/key.pem", __FILE__),
      certfile: File.expand_path("../../../fixtures/ssl/cert.pem", __FILE__)
  end

  def setup_all do
    App.run(verbose: false)
  end

  def teardown_all do
    Dynamo.Cowboy.shutdown App
  end

  test :http do
    assert { :ok, 200, _, client } = :hackney.request(:get, "http://127.0.0.1:8021/", [], "", [])
    assert { :ok, "scheme: http", _ } = :hackney.body(client)
  end

  test :https do
    assert { :ok, 200, _, client } = :hackney.request(:get, "https://127.0.0.1:8022/", [], "", [])
    assert { :ok, "scheme: https", _ } = :hackney.body(client)
  end
end
