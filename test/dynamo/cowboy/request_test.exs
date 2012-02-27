Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo::Cowboy::RequestTest do
  use ExUnit::Case

  def setup_all do
    Dynamo::Cowboy.run __MODULE__, port: 8011, verbose: false
  end

  def teardown_all do
    Dynamo::Cowboy.shutdown __MODULE__
  end

  def service(req, res) do
    function = binary_to_atom hd(req.path_segments), :utf8
    apply __MODULE__, function, [req, res]
  rescue: exception
    res.reply(500, [], exception.message <> inspect(Code.stacktrace))
  end

  # Tests

  def path_segments_0(req, res) do
    assert_equal ["path_segments_0"], req.path_segments
    res
  end

  def path_segments_1(req, res) do
    assert_equal ["path_segments_1", "foo", "bar", "baz"], req.path_segments
    res
  end

  def path_0(req, res) do
    assert_equal "/path_0", req.path
    res
  end

  def path_1(req, res) do
    assert_equal "/path_1/foo/bar/baz", req.path
    res
  end

  def mount(req, res) do
    assert_equal ["mount", "foo", "bar", "baz"], req.path_segments

    req = req.mount(["foo", "bar", "baz"])

    assert_equal "/foo/bar/baz", req.path
    assert_equal ["foo", "bar", "baz"], req.path_segments

    assert_equal "/mount/foo/bar/baz", req.full_path
    assert_equal ["mount", "foo", "bar", "baz"], req.full_path_segments

    assert_equal "/mount", req.script_info
    assert_equal ["mount"], req.script_info_segments

    req = req.mount(["bar", "baz"])

    assert_equal "/bar/baz", req.path
    assert_equal ["bar", "baz"], req.path_segments

    assert_equal "/mount/foo/bar/baz", req.full_path
    assert_equal ["mount", "foo", "bar", "baz"], req.full_path_segments

    assert_equal "/mount/foo", req.script_info
    assert_equal ["mount", "foo"], req.script_info_segments

    res
  end

  # Triggers

  test :path_segments do
    assert_success http_client.request :get, "/path_segments_0"
    assert_success http_client.request :get, "/path_segments_1/foo/bar/baz"
  end

  test :path do
    assert_success http_client.request :get, "/path_0"
    assert_success http_client.request :get, "/path_1/foo/bar/baz"
  end

  test :mount_suffix do
    assert_success http_client.request :get, "/mount/foo/bar/baz"
  end

  defp assert_success({ status, _, _ }) when div(status, 100) == 2 do
    :ok
  end

  defp assert_success({ status, _, body }) do
    assert false, "Expected successful response, got status #{inspect status} with body #{inspect body}"
  end

  defp http_client do
    HTTPClient.new("http://127.0.0.1:8011")
  end
end

