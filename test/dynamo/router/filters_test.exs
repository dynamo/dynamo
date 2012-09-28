Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.FiltersTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule PrepareFilter do
    def prepare(conn) do
      conn.assign(:value, 3)
    end
  end

  defmodule PrepareApp do
    use Dynamo.Router
    filter PrepareFilter

    get "/foo" do
      conn.resp(200, "OK")
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
      conn.resp(200, "OK")
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
      conn.resp(200, "OK")
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
      conn.resp(200, "OK")
    end
  end

  test "prepend filter" do
    conn = process(ChainApp, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200
  end

  defmodule ParamFilter do
    def prepare(conn) do
      value = conn.params["value"]
      conn.assign(:value, value)
    end
  end

  defmodule ParamsApp do
    use Dynamo.Router

    fetch [ :params ]

    filter ParamFilter

    get "/foo" do
      conn.resp(200, "OK")
    end
  end

  test "fetch before filter" do
    conn = process(ParamsApp, :GET, "/foo?value=bar")
    assert conn.assigns[:value] == "bar"
    assert conn.status == 200
  end
end
