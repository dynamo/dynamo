Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.RenderingTest do
  use ExUnit.Case
  use Dynamo.HTTP.Case

  defmodule RenderingRouter do
    use Dynamo.Router

    get "/set_content_type" do
      render conn.resp_content_type("application/json"), "hello.html"
    end

    get "/with_layout" do
      render conn, "hello.html", layout: "application.html"
    end

    get "/:template" do
      render conn.assign(:hello, "world"), template
    end
  end

  defmodule RenderingApp do
    @dynamo_registration false
    use Dynamo.App
    endpoint RenderingRouter

    config :dynamo,
      view_paths: [File.expand_path("../../../fixtures/views", __FILE__)]
  end

  def setup_all do
    Dynamo.app(RenderingApp)
  end

  def teardown_all do
    Dynamo.app(nil)
  end

  @app RenderingRouter

  test "sets response body and content type" do
    conn = get("/hello.html")
    assert conn.status == 200
    assert conn.resp_body == "HELLO!"
    assert conn.resp_content_type == "text/html"
  end

  test "ignores content type if one is already set" do
    conn = get("/set_content_type")
    assert conn.status == 200
    assert conn.resp_content_type == "application/json"
  end

  test "works with assigns" do
    conn = get("/assigns.html")
    assert conn.resp_body == "world"
    assert conn.resp_content_type == "text/html"
  end

  test "returns the updated connection" do
    conn = get("/updatable.html")
    assert conn.resp_body == "UPDATE\n\nCONN"
    assert conn.assigns[:template] == :eex
  end

  test "works with layouts" do
    conn = get("/with_layout")
    assert conn.resp_body == "<html>\nHELLO!\n</html>"
    assert conn.resp_content_type == "text/html"
  end

  test "raises on invalid template" do
    assert_raise Dynamo.View.TemplateNotFound, fn ->
      get("/unknown.html")
    end
  end
end
