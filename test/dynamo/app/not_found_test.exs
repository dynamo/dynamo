Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.App.NotFoundTest do
  use ExUnit.Case, async: true

  defrecord Mock, state: :unset, status: nil, path_info_segments: [], method: :GET do
    def resp(status, _body, mock) do
      mock.state(:set).status(status)
    end

    def forward_to(_, _, mock) do
      mock
    end
  end

  defmodule Router do
    use Dynamo.Router

    get "/it_works" do
      conn.resp(200, "Works")
    end
  end

  defmodule DefaultApp do
    use Dynamo.App
    forward "/", to: Router
  end

  defmodule App do
    use Dynamo.App

    forward "/", to: Router

    def not_found(conn) do
      conn.resp(302, "Other")
    end
  end

  test "cascades not found accesses" do
    assert DefaultApp.service(Mock.new).status == 404
    assert DefaultApp.service(Mock.new(path_info_segments: ["it_works"])).status == 200
    assert App.service(Mock.new).status == 302
    assert App.service(Mock.new(path_info_segments: ["it_works"])).status == 200
  end
end
