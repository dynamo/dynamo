Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.FetchTest do
  use ExUnit.Case, async: true

  use Dynamo.HTTP.Case

  defmodule FetchRouter do
    use Dynamo.Router

    fetch [:cookies, :params]

    prepare do
      conn.assign(:from_callback, conn.fetched)
    end

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  @app FetchRouter

  test "fetch with router" do
    conn = get("/foo")
    assert conn.fetched == [:params, :cookies]
    assert conn.assigns[:from_callback] == [:params, :cookies]
  end
end
