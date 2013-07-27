defmodule Dynamo.BaseTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule App do
    use Dynamo.Base

    config :dynamo, static_root: :app, endpoint: Dynamo.BaseTest
    config :linq,   adapter: :pg
    config :dynamo, static_root: :myapp

    initializer :sample do
      Process.put(__MODULE__, :sample)
    end

    def start do
      run_initializers
    end
  end

  def service(conn) do
    conn.assign(:done, :ok).resp_body("OK")
  end

  @endpoint App

  test "allows initializers to be run" do
    assert Process.get(App) == nil
    App.start
    assert Process.get(App) == :sample
  end

  test "sets and overrides config" do
    assert App.config[:dynamo][:static_root]  == :myapp
    assert App.config[:linq]                  == [adapter: :pg]
    assert App.config[:other]                 == nil
  end
end