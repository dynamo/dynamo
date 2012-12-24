Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.HTTP.RenderTest do
  defmodule RenderingRouter do
    use Dynamo.Router

    get "/set_content_type" do
      render conn.resp_content_type("application/json"), "hello.html"
    end

    get "/with_layout" do
      render conn, "content_for.html", layout: "yield.html"
    end

    get "/with_nested" do
      render conn, "nested.html", layout: "yield.html"
    end

    get "/:template" do
      render conn.assign(:hello, "world"), template
    end
  end

  defmodule RenderingApp do
    use Dynamo

    config :dynamo,
      endpoint: RenderingRouter,
      templates_paths: [File.expand_path("../../../fixtures/templates", __FILE__)]

    templates do
      import List, only: [flatten: 1]
      use Dynamo.Helpers
    end
  end

  use ExUnit.Case
  use Dynamo.HTTP.Case

  def setup_all do
    RenderingApp.start_link
    Dynamo.under_test(RenderingApp)
  end

  def teardown_all do
    Dynamo.under_test(nil)
  end

  @endpoint RenderingRouter

  test "sets response body and content type" do
    conn = get("/hello.html")
    assert conn.status == 200
    assert conn.sent_body == "HELLO!"
    assert conn.resp_content_type == "text/html"
  end

  test "ignores content type if one is already set" do
    conn = get("/set_content_type")
    assert conn.status == 200
    assert conn.resp_content_type == "application/json"
  end

  test "works with assigns" do
    conn = get("/assigns.html")
    assert conn.sent_body == "world"
    assert conn.resp_content_type == "text/html"
  end

  test "uses app prelude" do
    conn = get("/prelude.html")
    assert conn.sent_body == (?H /> integer_to_binary)
    assert conn.resp_content_type == "text/html"
  end

  test "returns the updated connection" do
    conn = get("/updatable.html")
    assert conn.sent_body == "UPDATE\n\nCONN"
    assert conn.assigns[:template] == :eex
  end

  test "works with layouts" do
    conn = get("/with_layout")
    assert strip_lines(conn.sent_body) == """
    <html>
    <head>
    <title>
    My Title
    </title>
    </head>
    <body>
    This is the intro
    Template body
    My footer
    </html>
    """
    assert conn.resp_content_type == "text/html"
  end

  test "works with nested rendering and layouts" do
    conn = get("/with_nested")
    assert strip_lines(conn.sent_body) == """
    <html>
    <head>
    <title>
    My Title
    </title>
    </head>
    <body>
    This is the intro
    NESTED
    Template body
    My footer
    </html>
    """
    assert conn.resp_content_type == "text/html"
  end


  test "raises on invalid template" do
    assert_raise Dynamo.TemplateNotFound, fn ->
      get("/unknown.html")
    end
  end

  defp strip_lines(body) do
    Regex.replace %r"\n+\s*", body, "\n"
  end
end
