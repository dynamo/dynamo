defmodule Dynamo.Cowboy.SSLTest do
  use ExUnit.Case, async: true

  defmodule App do
    use Dynamo
    use Dynamo.Router

    get "/scheme" do
      conn.send(200, "scheme: " <> atom_to_binary(conn.scheme))
    end

    get "/host_url" do
      conn.send(200, "host_url: " <> conn.host_url)
    end

    get "/port" do
      conn.send(200, "port: " <> integer_to_binary(conn.port))
    end

    get "/host" do
      conn.send(200, "host: " <> conn.host)
    end

    config :server, port: 8021

    config :ssl,
      port: 8022,
      password: "cowboy",
      keyfile: Path.expand("../../../fixtures/ssl/key.pem", __FILE__),
      certfile: Path.expand("../../../fixtures/ssl/cert.pem", __FILE__)
  end

  setup_all do
    App.run(verbose: false)
    :ok
  end

  teardown_all do
    Dynamo.Cowboy.shutdown App
    :ok
  end

  test :http_scheme do
    assert { :ok, 200, _, client } = :hackney.request(:get, "http://127.0.0.1:8021/scheme", [], "", [])
    assert { :ok, "scheme: http" } = :hackney.body(client)
  end

  test :http_host_url do
    assert { :ok, 200, _, client } = :hackney.request(:get, "http://127.0.0.1:8021/host_url", [], "", [])
    assert { :ok, "host_url: http://127.0.0.1:8021" } = :hackney.body(client)
  end

  test :http_host do
    assert { :ok, 200, _, client } = :hackney.request(:get, "http://127.0.0.1:8021/host", [], "", [])
    assert { :ok, "host: 127.0.0.1" } = :hackney.body(client)
  end

  test :http_port do
    assert { :ok, 200, _, client } = :hackney.request(:get, "http://127.0.0.1:8021/port", [], "", [])
    assert { :ok, "port: 8021" } = :hackney.body(client)
  end

  test :https_scheme do
    assert { :ok, 200, _, client } = :hackney.request(:get, "https://127.0.0.1:8022/scheme", [], "", [])
    assert { :ok, "scheme: https" } = :hackney.body(client)
  end

  test :https_host_url do
    assert { :ok, 200, _, client } = :hackney.request(:get, "https://127.0.0.1:8022/host_url", [], "", [])
    assert { :ok, "host_url: https://127.0.0.1:8022" } = :hackney.body(client)
  end

  test :https_host do
    assert { :ok, 200, _, client } = :hackney.request(:get, "https://127.0.0.1:8022/host", [], "", [])
    assert { :ok, "host: 127.0.0.1" } = :hackney.body(client)
  end

  test :https_port do
    assert { :ok, 200, _, client } = :hackney.request(:get, "https://127.0.0.1:8022/port", [], "", [])
    assert { :ok, "port: 8022" } = :hackney.body(client)
  end
end
