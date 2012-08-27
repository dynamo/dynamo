Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.AppTest do
  use ExUnit.Case, async: true
  import Dynamo.Router.TestHelpers

  defmodule App do
    use Dynamo.App
    endpoint Dynamo.AppTest

    config :dynamo, public_root: File.expand_path("../../fixtures", __FILE__)
  end

  Dynamo.app(nil)
  @app App

  test "cascades not found accesses" do
    conn = get("/public/file.txt")
    assert conn.status == 200
    assert conn.resp_body == "HELLO"
    assert conn.resp_headers["Content-Type"] == "text/plain"
  end
end
