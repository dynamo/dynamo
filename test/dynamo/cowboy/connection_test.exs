Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Cowboy.ConnectionTest do
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
  # rescue
  #   exception ->
  #     res.reply(500, [], exception.message <> inspect(Code.stacktrace))
  end

  # Request API

  def version(conn) do
    assert conn.version == { 1, 1 }
    conn
  end

  test :version do
    assert_success request :get, "/version"
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

  def res_cookies_0(conn) do
    assert conn.res_cookies == []

    conn = conn.set_cookie(:foo, :bar, path: "/hello")
    assert conn.res_cookies == [{ "foo", "bar", path: "/hello" }]

    conn = conn.set_cookie(:bar, :baz, http_only: false)
    conn.reply(200, [], "Hello")
  end

  def req_res_cookies(conn) do
    conn = conn.fetch(:cookies)
    assert conn.cookies["foo"] == "bar"
    assert conn.cookies["baz"] == "bat"

    conn = conn.set_cookie(:foo, :old)
    assert conn.cookies["foo"] == "old"
    assert conn.cookies["baz"] == "bat"

    conn = conn.set_cookie(:foo, :new)
    assert conn.cookies["foo"] == "new"
    conn.reply(200, [], "Hello")
  end

  test :req_cookies do
    assert_success request :get, "/req_cookies_0", [{ "Cookie", %b(foo="bar"; baz="bat") }]
    assert_success request :get, "/req_cookies_1", [{ "Cookie", %b(foo="bar"; baz="bat") }]
  end

  test :res_cookies do
    response = request :get, "/res_cookies_0"
    assert_success response

    { _, headers, _ } = response
    assert List.keyfind(headers, "Set-Cookie", 1) == { "Set-Cookie", "foo=bar; Version=1; Path=/hello; HttpOnly" }

    headers = List.keydelete(headers, "Set-Cookie", 1)
    assert List.keyfind(headers, "Set-Cookie", 1) == { "Set-Cookie","bar=baz; Version=1" }
  end

  test :req_res_cookies do
    response = request :get, "/req_res_cookies", [{ "Cookie", %b(foo="bar"; baz="bat") }]
    assert_success response

    { _, headers, _ } = response
    assert List.keyfind(headers, "Set-Cookie", 1) == { "Set-Cookie", "foo=new; Version=1; HttpOnly" }

    headers = List.keydelete(headers, "Set-Cookie", 1)
    assert List.keyfind(headers, "Set-Cookie", 1) == nil
  end

  ## Misc

  def forward_to(conn) do
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    conn = conn.forward_to ["foo", "bar", "baz"], Foo

    assert conn.path_info == "/foo/bar/baz"
    assert conn.path_info_segments == ["foo", "bar", "baz"]

    assert conn.path == "/forward_to/foo/bar/baz"
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert conn.script_info == "/forward_to"
    assert conn.script_info_segments == ["forward_to"]

    conn = conn.forward_to ["bar", "baz"], Bar

    assert conn.path_info == "/bar/baz"
    assert conn.path_info_segments == ["bar", "baz"]

    assert conn.path == "/forward_to/foo/bar/baz"
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert conn.script_info == "/forward_to/foo"
    assert conn.script_info_segments == ["forward_to", "foo"]

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