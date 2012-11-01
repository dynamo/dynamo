Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Cowboy.HTTPTest do
  use ExUnit.Case, async: true

  def setup_all do
    Dynamo.Cowboy.run __MODULE__, port: 8011, verbose: false
  end

  def teardown_all do
    Dynamo.Cowboy.shutdown __MODULE__
  end

  def service(conn) do
    function = binary_to_atom hd(conn.path_segments), :utf8
    apply __MODULE__, function, [conn]
  rescue
    exception ->
      conn.send(500, exception.message <> "\n" <> Exception.formatted_stacktrace)
  end

  # Request API

  def version(conn) do
    assert conn.version == { 1, 1 }
    conn
  end

  test :version do
    assert_success request :get, "/version"
  end

  def method(conn) do
    assert conn.method == "GET"
    assert conn.original_method == "GET"

    conn = conn.method("POST")
    assert conn.method == "POST"
    assert conn.original_method == "GET"

    conn
  end

  test :method do
    assert_success request :get, "/method"
  end

  def path_segments_0(conn) do
    assert conn.path_segments == ["path_segments_0"]
    conn
  end

  def path_segments_1(conn) do
    assert conn.path_segments == ["path_segments_1", "foo", "bar", "baz"]
    conn
  end

  test :path_segments do
    assert_success request :get, "/path_segments_0"
    assert_success request :get, "/path_segments_1/foo/bar/baz"
  end

  def path_0(conn) do
    assert conn.path == "/path_0"
    conn
  end

  def path_1(conn) do
    assert conn.path == "/path_1/foo/bar/baz"
    conn
  end

  test :path do
    assert_success request :get, "/path_0"
    assert_success request :get, "/path_1/foo/bar/baz"
  end

  def query_string_0(conn) do
    assert conn.query_string == "hello=world&foo=bar"
    conn
  end

  def query_string_1(conn) do
    assert conn.query_string == ""
    conn
  end

  test :query_string do
    assert_success request :get, "/query_string_0?hello=world&foo=bar"
    assert_success request :get, "/query_string_1"
  end

  def req_body(conn) do
    conn = conn.fetch(:body)
    assert conn.req_body == "foobar"
    conn
  end

  test :req_body do
    assert_success request :post, "/req_body", [{ "Content-Type", "application/x-foobar" }], "foobar"
  end

  def params_0(conn) do
    conn = conn.fetch(:params)
    assert conn.params[:hello]   == "world"
    assert conn.params[:foo]     == "bar"
    assert conn.params[:unknown] == nil
    conn
  end

  def params_1(conn) do
    conn = conn.fetch(:params)
    assert conn.params[:name] == "hello"

    file = conn.params[:pic]
    assert file.body == "hello\n\n"
    assert file.name == "pic"
    assert file.content_type == "text/plain"
    assert file.filename == "foo.txt"

    conn
  end

  test :params do
    assert_success request :get,  "/params_0?hello=world&foo=bar"
    assert_success request :post, "/params_0", [{ "Content-Type", "application/x-www-form-urlencoded" }], "hello=world&foo=bar"

    multipart = "------WebKitFormBoundaryw58EW1cEpjzydSCq\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\nhello\r\n------WebKitFormBoundaryw58EW1cEpjzydSCq\r\nContent-Disposition: form-data; name=\"pic\"; filename=\"foo.txt\"\r\nContent-Type: text/plain\r\n\r\nhello\n\n\r\n------WebKitFormBoundaryw58EW1cEpjzydSCq\r\nContent-Disposition: form-data; name=\"commit\"\r\n\r\nCreate User\r\n------WebKitFormBoundaryw58EW1cEpjzydSCq--\r\n"
    headers   = [
      { "Content-Type", "multipart/form-data; boundary=----WebKitFormBoundaryw58EW1cEpjzydSCq" },
      { "Content-Length", size(multipart) }
    ]

    assert_success request :get, "/params_1", headers, multipart
  end

  ## Cookies

  def req_cookies_0(conn) do
    conn = conn.fetch(:cookies)
    assert conn.req_cookies["foo"] == "bar"
    assert conn.req_cookies["baz"] == "bat"
    conn
  end

  def req_cookies_1(conn) do
    conn = conn.fetch(:cookies)
    assert conn.cookies["foo"] == "bar"
    assert conn.cookies["baz"] == "bat"
    conn
  end

  def resp_cookies_0(conn) do
    assert conn.resp_cookies == []

    conn = conn.set_cookie(:foo, :bar, path: "/hello")
    assert conn.resp_cookies == [{ "foo", "bar", path: "/hello" }]

    conn = conn.set_cookie(:bar, :baz, http_only: false)
    conn.send(200, "Hello")
  end

  def req_resp_cookies(conn) do
    conn = conn.fetch(:cookies)
    assert conn.cookies["foo"] == "bar"
    assert conn.cookies["baz"] == "bat"

    conn = conn.set_cookie(:foo, :new)
    assert conn.cookies["foo"] == "new"
    assert conn.cookies["baz"] == "bat"

    conn = conn.fetch(:cookies)
    assert conn.cookies["foo"] == "new"

    conn = conn.delete_cookie(:foo)
    assert conn.cookies["foo"] == nil
    conn.send(200, "Hello")
  end

  test :req_cookies do
    assert_success request :get, "/req_cookies_0", [{ "Cookie", %b(foo="bar"; baz="bat") }]
    assert_success request :get, "/req_cookies_1", [{ "Cookie", %b(foo="bar"; baz="bat") }]
  end

  test :resp_cookies do
    response = request :get, "/resp_cookies_0"
    assert_success response

    { _, headers, _ } = response
    assert List.keyfind(headers, "set-cookie", 0) == { "set-cookie", "foo=bar; path=/hello; HttpOnly" }

    headers = List.keydelete(headers, "set-cookie", 0)
    assert List.keyfind(headers, "set-cookie", 0) == { "set-cookie","bar=baz" }
  end

  test :req_resp_cookies do
    response = request :get, "/req_resp_cookies", [{ "Cookie", %b(foo="bar"; baz="bat") }]
    assert_success response

    { _, headers, _ } = response
    { "set-cookie", contents } = List.keyfind(headers, "set-cookie", 0)
    assert contents =~ %r"foo=; expires=Thu, 01 Jan 1970 12:00:00 GMT; max-age=0; HttpOnly"

    headers = List.keydelete(headers, "set-cookie", 0)
    assert List.keyfind(headers, "set-cookie", 0) == nil
  end

  ## Assigns

  def assigns(conn) do
    assert conn.assigns == []

    conn = conn.assign :foo, "bar"
    assert conn.assigns == [foo: "bar"]

    conn = conn.assign :foo, "baz"
    assert conn.assigns == [foo: "baz"]

    conn
  end

  test :assigns do
    assert_success request :get, "/assigns"
  end

  ## Headers

  def req_headers(conn) do
    conn = conn.fetch(:headers)
    assert conn.req_headers["host"] == "127.0.0.1:8011"
    assert conn.req_headers["x-special"] == "foo"
    assert conn.req_headers["x-upcase"] == "bar"
    conn
  end

  test :req_headers do
    assert_success request :get, "/req_headers", [{ "X-Special", "foo" }, { "X-Upcase", "bar" }]
  end

  def resp_headers(conn) do
    assert conn.resp_headers == Binary.Dict.new

    conn = conn.set_resp_header("x-header", "First")
    assert conn.resp_headers["x-header"] == "First"

    conn = conn.set_resp_header("x-header", "Second")
    assert conn.resp_headers["x-header"] == "Second"

    conn = conn.delete_resp_header("x-header")
    assert conn.resp_headers["x-header"] == nil

    conn = conn.set_resp_header("x-header", "Third")
    assert conn.resp_headers["x-header"] == "Third"

    conn.send(200, "Hello")
  end

  test :resp_headers do
    response = request :get, "/resp_headers"
    assert_success response

    { _, headers, _ } = response
    assert List.keyfind(headers, "x-header", 0) == { "x-header", "Third" }
  end

  def no_content_type_and_charset(conn) do
    conn.send(200, "Hello")
  end

  def resp_content_type_and_charset(conn) do
    assert conn.resp_charset == "utf-8"

    conn = conn.resp_content_type("application/json")
    assert conn.resp_content_type == "application/json"

    conn.send(200, "Hello")
  end

  test :resp_content_type_and_charset do
    response = request :get, "/no_content_type_and_charset"
    assert_success response

    { _, headers, _ } = response
    assert List.keyfind(headers, "content-type", 0) == nil

    response = request :get, "/resp_content_type_and_charset"
    assert_success response

    { _, headers, _ } = response
    assert List.keyfind(headers, "content-type", 0) == { "content-type", "application/json; charset=utf-8" }
  end

  ## Response API

  def send(conn) do
    assert conn.state == :unset

    conn = conn.send(201, "OK")
    assert conn.state  == :sent
    assert conn.status == 201

    conn
  end

  def send_with_head(conn) do
    assert_raise Dynamo.HTTP.InvalidSendOnHeadError, fn ->
      conn.send(200, "HELLO")
    end

    conn.status(201)
  end

  test :send do
    assert { 201, _, "OK" } = request :get, "/send"
    assert { 201, _, "" }   = request :head, "/send_with_head"
  end

  def sendfile(conn) do
    file = File.expand_path("../../../fixtures/static/file.txt", __FILE__)
    conn = conn.sendfile(file)
    assert conn.state  == :sent
    assert conn.status == 200
    conn
  end

  def invalid_sendfile(conn) do
    conn = conn.sendfile("unknown.file")
  end

  test :sendfile do
    { 200, headers, "HELLO" } = request :get, "/sendfile"
    assert List.keyfind(headers, "content-length", 0) == { "content-length", "5" }

    assert { 500, _, _ } = request :get, "/invalid_sendfile"
  end

  def resp(conn) do
    assert conn.state == :unset

    conn = conn.resp(201, "OK")
    assert conn.state     == :set
    assert conn.status    == 201
    assert conn.resp_body == "OK"

    conn = conn.resp(302, "Redirected")
    assert conn.state     == :set
    assert conn.status    == 302
    assert conn.resp_body == "Redirected"

    conn
  end

  test :resp do
    assert { 302, _, "Redirected" } = request :get, "/resp"
  end

  ## Misc

  def forward_to(conn) do
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

  test :forward_to do
    assert_success request :get, "/forward_to/foo/bar/baz"
  end

  # Helpers

  defp assert_success({ status, _, _ }) when status in 200..299 do
    :ok
  end

  defp assert_success({ status, _, body }) do
    flunk "Expected successful response, got status #{inspect status} with body #{inspect body}"
  end

  defp request(verb, path, headers // [], body // "") do
    { :ok, status, headers, client } =
      :hackney.request(verb, "http://127.0.0.1:8011" <> path, headers, body, [])
    { :ok, body, _ } = :hackney.body(client)
    { status, headers, body }
  end
end