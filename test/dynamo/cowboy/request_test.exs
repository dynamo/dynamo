Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Cowboy.RequestTest do
  use ExUnit.Case

  alias Dynamo.Cowboy.Request, as: Req

  def setup_all do
    Dynamo.Cowboy.run __MODULE__, port: 8011, verbose: false
  end

  def teardown_all do
    Dynamo.Cowboy.shutdown __MODULE__
  end

  def service(req, res) do
    function = binary_to_atom hd(req.path_segments), :utf8
    apply __MODULE__, function, [req, res]
  # rescue
  #   exception ->
  #     res.reply(500, [], exception.message <> inspect(Code.stacktrace))
  end

  # Tests

  def path_segments_0(req, res) do
    assert req.path_segments == ["path_segments_0"]
    res
  end

  def path_segments_1(req, res) do
    assert req.path_segments == ["path_segments_1", "foo", "bar", "baz"]
    res
  end

  def path_0(req, res) do
    assert req.path == "/path_0"
    res
  end

  def path_1(req, res) do
    assert req.path == "/path_1/foo/bar/baz"
    res
  end

  def query_string_0(req, res) do
    assert req.query_string == "hello=world&foo=bar"
    res
  end

  def query_string_1(req, res) do
    assert req.query_string == ""
    res
  end

  def params_0(req, res) do
    assert req.params[:hello]   == "world"
    assert req.params[:foo]     == "bar"
    assert req.params[:unknown] == nil
    res
  end

  def forward_to(req, res) do
    assert req.path_segments == ["forward_to", "foo", "bar", "baz"]

    req = Req.forward_to req, ["foo", "bar", "baz"], Foo

    assert req.path_info == "/foo/bar/baz"
    assert req.path_info_segments == ["foo", "bar", "baz"]

    assert req.path == "/forward_to/foo/bar/baz"
    assert req.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert req.script_info == "/forward_to"
    assert req.script_info_segments == ["forward_to"]

    req = Req.forward_to req, ["bar", "baz"], Bar

    assert req.path_info == "/bar/baz"
    assert req.path_info_segments == ["bar", "baz"]

    assert req.path == "/forward_to/foo/bar/baz"
    assert req.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert req.script_info == "/forward_to/foo"
    assert req.script_info_segments == ["forward_to", "foo"]

    res
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