defmodule Dynamo.HTTP.RedirectTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case
  import Dynamo.HTTP.Redirect

  test :redirect do
    conn = redirect conn(:GET, "/"), to: "/foo"
    assert conn.resp_headers["location"] == "http://127.0.0.1/foo"
    assert conn.status == 302
  end

  test :redirect! do
    assert { :halt!, conn } = catch_throw(redirect! conn(:GET, "/"), to: "/foo")
    assert conn.resp_headers["location"] == "http://127.0.0.1/foo"
    assert conn.status == 302
  end

  test :redirect_with_status do
    conn = redirect conn(:GET, "/"), to: "/foo", status: 301
    assert conn.resp_headers["location"] == "http://127.0.0.1/foo"
    assert conn.status == 301
  end

  test :redirect_with_format do
    conn = redirect conn(:GET, "/"), to: "/foo", format: :html
    assert conn.resp_headers["location"] == "http://127.0.0.1/foo"
    assert conn.resp_content_type == "text/html"
  end

  test :redirect_with_set_content_type do
    conn = redirect conn(:GET, "/").resp_content_type("text/html"), to: "/foo"
    assert conn.resp_headers["location"] == "http://127.0.0.1/foo"
    assert conn.resp_body =~ ~r"redirected"
  end

  test :redirect_with_full_url do
    conn = redirect conn(:GET, "/"), to: "http://google.com/foo"
    assert conn.resp_headers["location"] == "http://google.com/foo"
  end
end
