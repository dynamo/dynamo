Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.App.ConfigTest do
  use ExUnit.Case, async: true

  import Dynamo.Router.TestHelpers

  defmodule App do
    use Dynamo.App

    endpoint Dynamo.App.ConfigTest

    config :dynamo,
      public_root:  :app,
      public_route: "/public",
      root: File.expand_path("../../../fixtures", __FILE__)

    config :linq, adapter: :pg

    config :dynamo,
      public_root: :myapp
  end

  Dynamo.app(nil)

  defmodule DefaultApp do
    use Dynamo.App
    endpoint App
  end

  Dynamo.app(nil)

  def service(conn) do
    conn.assign(:done, :ok).resp(200, "OK")
  end

  @app App

  test "defines an endpoint" do
    assert get("/").assigns[:done] == :ok
  end

  test "defines a root" do
    assert App.config[:dynamo][:root] == File.expand_path("../../../fixtures", __FILE__)
    assert DefaultApp.config[:dynamo][:root] == File.expand_path("../..", __FILE__)
  end

  test "sets and overrides config" do
    assert App.config[:dynamo][:public_root]  == :myapp
    assert App.config[:dynamo][:public_route] == "/public"
    assert App.config[:linq]                  == [adapter: :pg]
    assert App.config[:other]                 == nil
  end

  test "gets config from environment" do
    assert App.config[:from_dev][:other]  == "config"
  end
end
