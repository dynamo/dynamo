Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo::Cowboy::RouterApp do
  use Dynamo::Router

  get "/foo/bar" do
    request.reply(200, [], "Hello World!")
  end
end

defmodule Dynamo::Cowboy::RouterTest do
  use ExUnit::Case

  def setup_all do
    Dynamo::Cowboy.run RouterApp, port: 8012
  end

  def teardown_all do
    Dynamo::Cowboy.shutdown RouterApp
  end

  test "basic request on a router app" do
    assert_match { 200, _, "Hello World!" }, http_client.request :get, "/foo/bar"
  end

  defp http_client do
    HTTPClient.new("http://127.0.0.1:8012")
  end
end

