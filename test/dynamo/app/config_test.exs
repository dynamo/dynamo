Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.App.ConfigTest do
  use ExUnit.Case, async: true

  import Dynamo.Router.TestHelpers

  defmodule App do
    use Dynamo.App

    endpoint Dynamo.App.ConfigTest

    config :dynamo,
      public_root:  :app,
      public_route: "/public"

    config :linq, adapter: :pg

    config :dynamo,
      public_root: :myapp
  end

  def service(conn) do
    conn.assign(:done, :ok).resp(200, "OK")
  end

  @app App

  test "defines an endpoint" do
    assert get("/").assigns[:done] == :ok
  end

  test "sets and overrides config" do
    assert App.config[:dynamo][:public_root]  == :myapp
    assert App.config[:dynamo][:public_route] == "/public"
    assert App.config[:linq]                  == [adapter: :pg]
    assert App.config[:other]                 == nil
  end
end
