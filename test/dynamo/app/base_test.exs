Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.App.BaseTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule App do
    use Dynamo.App.Base

    endpoint Dynamo.App.BaseTest

    config :dynamo, static_root:  :app
    config :linq,   adapter: :pg
    config :dynamo, static_root: :myapp

    initializer :sample do
      Process.put(__MODULE__, :sample)
    end
  end

  def service(conn) do
    conn.assign(:done, :ok).resp_body("OK")
  end

  @app App

  test "defines a start which runs initializers" do
    assert Process.get(App) == nil
    App.start
    assert Process.get(App) == :sample
  end

  test "defines an endpoint" do
    assert get("/").assigns[:done] == :ok
  end

  test "sets and overrides config" do
    assert App.config[:dynamo][:static_root]  == :myapp
    assert App.config[:linq]                  == [adapter: :pg]
    assert App.config[:other]                 == nil
  end
end