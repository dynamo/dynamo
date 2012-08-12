Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Cowboy.ConnectionTest do
  use ExUnit.Case

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

  # Tests

  def path_segments_0(conn) do
    assert conn.path_segments == ["path_segments_0"]
    conn
  end

  def path_segments_1(conn) do
    assert conn.path_segments == ["path_segments_1", "foo", "bar", "baz"]
    conn
  end

  def path_0(conn) do
    assert conn.path == "/path_0"
    conn
  end

  def path_1(conn) do
    assert conn.path == "/path_1/foo/bar/baz"
    conn
  end

  def query_string_0(conn) do
    assert conn.query_string == "hello=world&foo=bar"
    conn
  end

  def query_string_1(conn) do
    assert conn.query_string == ""
    conn
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

  def cookies(conn) do
    conn = conn.fetch(:cookies)
    assert conn.cookies["foo"] == "bar"
    assert conn.cookies["baz"] == "bat"
    conn
  end

  # Triggers

  test :path_segments do
    assert_success request :get, "/path_segments_0"
    assert_success request :get, "/path_segments_1/foo/bar/baz"
  end

  test :path do
    assert_success request :get, "/path_0"
    assert_success request :get, "/path_1/foo/bar/baz"
  end

  test :query_string do
    assert_success request :get, "/query_string_0?hello=world&foo=bar"
    assert_success request :get, "/query_string_1"
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

  test :cookies do
    assert_success request :get, "/cookies", [{ "Cookie", %b(foo="bar"; baz="bat") }]
  end

  test :forward_to do
    assert_success request :get, "/forward_to/foo/bar/baz"
  end

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