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
  end

  # Tests

  def path_segments(req, res) do
    assert_equal ["path_segments"], req.path_segments
    res
  end

  # Triggers

  test :path_segments do
    assert_status 204, http_client.request :get, "/path_segments"
  end

  defp assert_status(expected, { actual, _, _ }) do
    assert_equal expected, actual, "Expected status code #{inspect expected} got #{inspect actual}"
  end

  defp http_client do
    HTTPClient.new("http://127.0.0.1:8011")
  end
end

