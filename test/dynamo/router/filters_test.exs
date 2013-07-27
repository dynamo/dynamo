defmodule Dynamo.Router.FiltersTest do
  use ExUnit.Case, async: true
  import Dynamo.HTTP.Case, only: [process: 3]

  defmodule PrepareFilter do
    def prepare(conn) do
      conn.assign(:value, 3)
    end
  end

  defmodule PrepareApp do
    use Dynamo.Router
    filter PrepareFilter

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  test "prepare filter" do
    conn = process(PrepareApp, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200
  end

  defmodule FinalizeFilter do
    def finalize(conn) do
      conn.assign(:value, 3)
    end
  end

  defmodule FinalizeApp do
    use Dynamo.Router
    filter FinalizeFilter

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  test "finalize filter" do
    conn = process(PrepareApp, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200
  end

  defmodule ServiceFilter do
    def service(conn, fun) do
      conn = fun.(conn.assign(:value, 3))
      conn.assign(:value, conn.assigns[:value] * 2)
    end
  end

  defmodule ServiceApp do
    use Dynamo.Router
    filter ServiceFilter

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  test "service filter" do
    conn = process(ServiceApp, :GET, "/foo")
    assert conn.assigns[:value] == 6
    assert conn.status == 200
  end

  defmodule PrependFilter do
    def prepare(conn) do
      conn.assign(:value, :not_used)
    end
  end

  defmodule ChainApp do
    use Dynamo.Router

    filter PrepareFilter
    prepend_filter PrependFilter

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  test "prepend filter" do
    conn = process(ChainApp, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200
  end
end
