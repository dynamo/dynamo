defmodule Dynamo.Cowboy.ConnectionTest do
  use ExUnit.Case, async: true

  setup_all do
    Dynamo.Cowboy.run __MODULE__, port: 8011, verbose: false
    :ok
  end

  teardown_all do
    Dynamo.Cowboy.shutdown __MODULE__
    :ok
  end

  def service(conn) do
    function = binary_to_atom hd(conn.path_segments)
    apply __MODULE__, function, [conn]
  rescue
    exception ->
      conn.send(500, exception.message <> "\n" <> Exception.format_stacktrace)
  end

  # Request API

  def version(conn) do
    assert conn.version == :"HTTP/1.1"
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

  def path_2(conn) do
    assert conn.path == "/path_2/foo/bar/baz/"
    conn
  end

  test :path do
    assert_success request :get, "/path_0"
    assert_success request :get, "/path_1/foo/bar/baz"
    assert_success request :get, "/path_2/foo/bar/baz/"
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

  def fetch_list(conn) do
    conn = conn.fetch([:body, :cookies])
    assert conn.req_body == "foobar"
    conn
  end

  test :fetch_list do
    assert_success request :post, "/fetch_list", [{ "Content-Type", "application/x-foobar" }], "foobar"
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
    assert File.read!(file.path) == "hello\n\n"
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
    import Dynamo.HTTP.Cookies, only: :functions

    conn = conn.fetch(:cookies)
    assert get_cookie(conn, "foo") == "bar"
    assert get_cookie(conn, "baz") == "bat"
    conn
  end

  def req_cookies_2(conn) do
    import Dynamo.HTTP.Cookies, only: :functions

    conn = conn.fetch(:cookies)
    assert get_cookie(conn, "foo") == "bar=baz"
    conn
  end

  def resp_cookies_0(conn) do
    import Dynamo.HTTP.Cookies, only: :functions

    assert conn.resp_cookies == []

    conn = put_cookie(conn, :foo, :bar, path: "/hello")
    assert conn.resp_cookies == [{ "foo", "bar", path: "/hello" }]

    conn = put_cookie(conn, :bar, :baz, http_only: false)
    conn.send(200, "Hello")
  end

  def req_resp_cookies(conn) do
    import Dynamo.HTTP.Cookies, only: :functions

    conn = conn.fetch(:cookies)
    assert get_cookie(conn, "foo") == "bar"
    assert get_cookie(conn, "baz") == "bat"

    conn = put_cookie(conn, :foo, :new)
    assert get_cookie(conn, "foo") == "new"
    assert get_cookie(conn, "baz") == "bat"

    conn = conn.fetch(:cookies)
    assert get_cookie(conn, "foo") == "new"

    conn = delete_cookie(conn, :foo)
    assert get_cookie(conn, :foo) == nil
    conn.send(200, "Hello")
  end

  test :req_cookies do
    assert_success request :get, "/req_cookies_0", [{ "Cookie", %s(foo=bar; baz=bat) }]
    assert_success request :get, "/req_cookies_1", [{ "Cookie", %s(foo=bar; baz=bat) }]
    assert_success request :get, "/req_cookies_2", [{ "Cookie", %s(foo=bar=baz) }]
  end

  test :resp_cookies do
    response = request :get, "/resp_cookies_0"
    assert_success response

    { _, headers, _ } = response
    assert List.keyfind(headers, "set-cookie", 0) == { "set-cookie", "foo=bar; path=/hello; HttpOnly" }

    headers = List.keydelete(headers, "set-cookie", 0)
    assert List.keyfind(headers, "set-cookie", 0) == { "set-cookie", "bar=baz; path=/" }
  end

  test :req_resp_cookies do
    response = request :get, "/req_resp_cookies", [{ "Cookie", %s(foo=bar; baz=bat) }]
    assert_success response

    { _, headers, _ } = response
    { "set-cookie", contents } = List.keyfind(headers, "set-cookie", 0)
    assert contents =~ %r"foo=; path=/; expires=Thu, 01 Jan 1970 12:00:00 GMT; max-age=0; HttpOnly"

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
    assert conn.resp_headers["cache-control"] == "max-age=0, private, must-revalidate"

    conn = conn.put_resp_header("x-header", "First")
    assert conn.resp_headers["x-header"] == "First"

    conn = conn.put_resp_header("x-header", "Second")
    assert conn.resp_headers["x-header"] == "Second"

    conn = conn.delete_resp_header("x-header")
    assert conn.resp_headers["x-header"] == nil

    conn = conn.put_resp_header("x-header", "Third")
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
    refute conn.already_sent?

    conn = conn.send(201, "OK")
    assert conn.state  == :sent
    assert conn.status == 201
    assert conn.already_sent?
    assert conn.already_sent?

    conn
  end

  def send_with_head(conn) do
    conn.send(200, "OK")
  end

  test :send do
    assert { 201, _, "OK" } = request :get, "/send"
    assert { 200, _, "" }   = request :head, "/send_with_head"
  end

  def send_chunked(conn) do
    conn = conn.send_chunked(200)
    { :ok, conn } = conn.chunk("1")
    { :ok, conn } = conn.chunk("2")
    { :ok, conn } = conn.chunk("3")
    conn
  end

  test :send_chunked do
    assert { 200, _, "123" } = request :get,  "/send_chunked"
  end

  def sendfile(conn) do
    file = Path.expand("../../../fixtures/static/file.txt", __FILE__)
    conn = conn.sendfile(200, file)
    assert conn.state  == :sent
    assert conn.status == 200
    conn
  end

  def invalid_sendfile(conn) do
    conn.sendfile(200, "unknown.file")
  end

  test :sendfile do
    { 200, headers, "HELLO" } = request :get, "/sendfile"
    assert List.keyfind(headers, "content-length", 0) == { "content-length", "5" }

    assert { 500, _, _ } = request :get, "/invalid_sendfile"
  end

  def before_send_with_send(conn) do
    conn.before_send(fn(conn) ->
      assert conn.state == :set
      assert conn.status == 201
      assert conn.resp_body == "CREATED"
      conn.resp(200, "OK")
    end).send(201, "CREATED")
  end

  test :before_send_with_send do
    assert { 200, _, "OK" } = request :get, "/before_send_with_send"
  end

  def before_send_with_sendfile(conn) do
    file = Path.expand("../../../fixtures/static/file.txt", __FILE__)
    conn.before_send(fn(conn) ->
      assert conn.state == :sendfile
      assert conn.status == 201
      conn.status(200)
    end).sendfile(201, file)
  end

  test :before_send_with_sendfile do
    { 200, _, "HELLO" } = request :get, "/before_send_with_sendfile"
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

    conn.send
  end

  test :resp do
    assert { 302, _, "Redirected" } = request :get, "/resp"
  end

  ## Misc

  def conn_inspect(conn) do
    conn.send(200, inspect(conn))
  end

  test :inspect do
    assert { 200, _, "#Dynamo.Connection<GET /conn_inspect (cowboy)>" } = request :get, "/conn_inspect"
  end

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
    { :ok, body } = :hackney.body(client)
    { status, headers, body }
  end
end
