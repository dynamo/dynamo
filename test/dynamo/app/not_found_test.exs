Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.App.NotFoundTest do
  use ExUnit.Case, async: true
  import Dynamo.Router.TestHelpers

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
    assert process(DefaultApp, :GET, "/").status == 404
    assert process(DefaultApp, :GET, "/it_works").status == 200
    assert process(App, :GET, "/").status == 302
    assert process(App, :GET, "/it_works").status == 200
  end
end
