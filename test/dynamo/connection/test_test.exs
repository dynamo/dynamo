defmodule Dynamo.Connection.TestTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  ## Request

  test :version do
    assert conn(:GET, "/").version == { 1, 1 }
  end

  test :method do
    assert conn(:GET, "/").method == "GET"
    assert conn(:POST, "/").method == "POST"

    conn = conn(:GET, "/")
    assert conn.method == "GET"
    assert conn.original_method == "GET"

    conn = conn.method("POST")
    assert conn.method == "POST"
    assert conn.original_method == "GET"
  end

  test :peer do
    conn = conn(:GET, "/foo/bar")
    conn = conn.peer({127, 0, 0, 1})
    assert conn.peer == {127, 0, 0, 1}
    assert conn.method == "GET"
    assert conn.path == "/foo/bar"
  end

  test :path do
    assert conn(:GET, "/foo/bar").path_segments == ["foo", "bar"]
    assert conn(:GET, "/").path_segments == []

    assert conn(:GET, "/foo/bar").path == "/foo/bar"
    assert conn(:GET, "/").path == "/"
    assert conn(:GET, "/foo/bar/").path == "/foo/bar/"
  end

  test :query_string do
    assert conn(:GET, "/foo/bar").query_string == ""
    assert conn(:GET, "/foo/bar?hello=world&foo=bar").query_string == "hello=world&foo=bar"
  end

  test :params do
    conn = conn(:GET, "/foo/bar?hello=world&foo[name]=bar")

    assert_raise Dynamo.Connection.UnfetchedError, fn ->
      conn.params
    end

    params = conn.fetch(:params).params
    assert params["hello"] == "world"
    assert params["foo"]["name"] == "bar"
  end

  test :req_headers do
    conn = conn(:GET, "/foo/bar")

    assert_raise Dynamo.Connection.UnfetchedError, fn ->
      conn.req_headers
    end

    conn = conn.put_req_header "X-Code", "123456"
    assert conn.fetch(:headers).req_headers["x-code"] == "123456"

    conn = conn.delete_req_header "X-Code"
    assert conn.fetch(:headers).req_headers["x-code"] == nil
  end

  test :host do
    conn = conn(:GET, "/foo/bar").fetch(:headers)
    assert conn.host == "127.0.0.1"
    assert conn.req_headers["host"] == "127.0.0.1"

    conn = conn(:GET, "//example.com:4000/foo/bar").fetch(:headers)
    assert conn.host == "example.com"
    assert conn.req_headers["host"] == "example.com:4000"
  end

  test :port do
    conn = conn(:GET, "/foo/bar").fetch(:headers)
    assert conn.port == 80

    conn = conn(:GET, "//example.com:4000/foo/bar").fetch(:headers)
    assert conn.port == 4000
  end

  test :host_url do
    conn = conn(:GET, "/foo/bar").fetch(:headers)
    assert conn.host_url == "http://127.0.0.1"

    conn = conn(:GET, "//example.com:4000/foo/bar").fetch(:headers)
    assert conn.host_url == "http://example.com:4000"
  end

  test :req_body do
    conn = conn(:POST, "/foo/bar", "foobar")

    assert_raise Dynamo.Connection.UnfetchedError, fn ->
      conn.req_body
    end

    assert conn.fetch(:body).req_body == "foobar"
  end

  test :fetch do
    conn = conn(:POST, "/foo/bar", "foobar").fetch([:cookies, :body])
    assert conn.req_body == "foobar"
    assert conn.req_cookies["foo"] == nil
  end

  ## Cookies

  test :req_cookies do
    conn = conn(:GET, "/").put_req_cookie(:foo, "bar").put_req_cookie(:baz, "bat").fetch(:cookies)
    assert conn.req_cookies["foo"] == "bar"
    assert conn.req_cookies["baz"] == "bat"
    conn
  end

  ## Response

  test :send do
    conn = conn(:GET, "/")
    assert conn.state == :unset
    refute conn.already_sent?

    conn = conn.send(201, "OK")
    assert conn.state  == :sent
    assert conn.status == 201

    # Call already_sent? twice to ensure it won't change.
    assert conn.already_sent?
    assert conn.already_sent?
  end

  test :before_send_with_send do
    conn = conn(:GET, "/").before_send(fn(conn) ->
      assert conn.state == :set
      assert conn.status == 201
      assert conn.resp_body == "CREATED"
      conn.resp(200, "OK")
    end).send(201, "CREATED")

    assert conn.state  == :sent
    assert conn.status == 200
    assert conn.sent_body == "OK"
  end

  test :send_with_head do
    conn = conn(:HEAD, "/")
    conn = conn.send(200, "HELLO")
    assert conn.sent_body == ""
  end

  test :send_chunked do
    conn = conn(:GET, "/")
    assert conn.state == :unset

    conn = conn.send_chunked(201)
    assert conn.state  == :chunked
    assert conn.status == 201
    assert conn.sent_body == ""

    { :ok, conn } = conn.chunk("1")
    { :ok, conn } = conn.chunk("2")
    { :ok, conn } = conn.chunk("3")

    assert conn.state  == :chunked
    assert conn.status == 201
    assert conn.sent_body == "123"
  end

  test :send_chunked_with_head do
    conn = conn(:HEAD, "/").send_chunked(201)
    assert conn.state  == :chunked
    assert conn.sent_body == ""

    { :ok, conn } = conn.chunk("1")
    { :ok, conn } = conn.chunk("2")
    { :ok, conn } = conn.chunk("3")

    assert conn.sent_body == ""
  end

  test :sendfile do
    file = Path.expand("../../../fixtures/static/file.txt", __FILE__)
    conn = conn(:GET, "/").sendfile(200, file)
    assert conn.state     == :sent
    assert conn.status    == 200
    assert conn.sent_body == "HELLO"
    conn
  end

  test :status do
    conn = conn(:GET, "/")
    assert conn.state == :unset

    conn = conn.status(201)
    assert conn.state  == :set
    assert conn.status == 201
  end

  test :resp_body do
    conn = conn(:GET, "/")
    assert conn.state == :unset

    conn = conn.resp_body("OK")
    assert conn.state     == :set
    assert conn.resp_body == "OK"
  end

  test :resp_content_type_and_charset do
    conn = conn(:GET, "/").send(200, "OK")
    assert conn.resp_headers["content-type"] == nil

    conn = conn(:GET, "/").resp_content_type("application/json").send(200, "OK")
    assert conn.resp_headers["content-type"] == "application/json; charset=utf-8"
  end

  ## Misc

  test :assigns do
    conn  = conn(:GET, "/")
    assert conn.assigns == []

    conn = conn.assign :foo, "bar"
    assert conn.assigns == [foo: "bar"]

    conn = conn.assign :foo, "baz"
    assert conn.assigns == [foo: "baz"]
  end

  test :private do
    conn  = conn(:GET, "/")
    assert is_list conn.private

    conn = conn.put_private :foo, "bar"
    assert conn.private[:foo] == "bar"

    conn = conn.put_private :foo, "baz"
    assert conn.private[:foo] == "baz"
  end

  test :forward_to do
    conn = conn(:GET, "/forward_to/foo/bar/baz")
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    conn = conn.forward_to ["forward_to", "foo", "bar", "baz"], Foo
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    conn = conn.forward_to ["foo", "bar", "baz"], Foo

    assert conn.path_info == "/foo/bar/baz"
    assert conn.path_info_segments == ["foo", "bar", "baz"]

    assert conn.path == "/forward_to/foo/bar/baz"
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert conn.script_name == "/forward_to"
    assert conn.script_name_segments == ["forward_to"]

    conn = conn.forward_to ["bar", "baz"], Bar

    assert conn.path_info == "/bar/baz"
    assert conn.path_info_segments == ["bar", "baz"]

    assert conn.path == "/forward_to/foo/bar/baz"
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert conn.script_name == "/forward_to/foo"
    assert conn.script_name_segments == ["forward_to", "foo"]

    conn = conn.forward_to [], Bar

    assert conn.path_info == "/"
    assert conn.path_info_segments == []

    assert conn.path == "/forward_to/foo/bar/baz"
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert conn.script_name == "/forward_to/foo/bar/baz"
    assert conn.script_name_segments == ["forward_to", "foo", "bar", "baz"]

    conn
  end
end
