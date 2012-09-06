Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.PrepareCallbacksTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule SingleCallbacks do
    use Dynamo.Router

    prepare :foo

    get "/foo" do
      conn.resp(200, "OK")
    end

    defp foo(conn) do
      conn.assign(:value, 3)
    end
  end

  test "dispatch single callback" do
    conn = process(SingleCallbacks, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200
  end

  defmodule Bar do
    def new do
      # We are returning a tuple with three elements
      # to ensure we are escaping the tuple contents
      # with Macro.escape when generating the callback
      { __MODULE__, 1, [] }
    end

    def service(conn) do
      conn.assign(:value, 3)
    end

    def service(conn, { __MODULE__, 1, [] }) do
      conn.assign(:value, conn.assigns[:value] * 2)
    end
  end

  defmodule DoubleCallbacks do
    use Dynamo.Router

    prepare Bar
    prepare Bar.new

    get "/foo" do
      conn.resp(200, "OK")
    end
  end

  test "dispatch double callback" do
    conn = process(DoubleCallbacks, :GET, "/foo")
    assert conn.assigns[:value] == 6
    assert conn.status == 200
  end

  defmodule BlockCallbacks do
    use Dynamo.Router

    prepare do
      conn.assign(:value, 3)
    end

    prepare do
      conn.assign(:value, conn.assigns[:value] * 2)
    end

    get "/foo" do
      conn.resp(200, "OK")
    end
  end

  test "dispatch block callback" do
    conn = process(DoubleCallbacks, :GET, "/foo")
    assert conn.assigns[:value] == 6
    assert conn.status == 200
  end

  defmodule InvalidCallbacks do
    use Dynamo.Router

    prepare do
      :ok
    end

    get "/foo" do
      conn.resp(200, "OK")
    end
  end

  test "invalid dispatch callback" do
    assert_raise Dynamo.Router.Callbacks.InvalidCallbackError, fn ->
      process(InvalidCallbacks, :GET, "/foo")
    end
  end

  defmodule AbortingCallbacks do
    use Dynamo.Router

    prepare do
      conn.resp(302, "Redirect")
    end

    get "/foo" do
      raise "will never be invoked"
    end
  end

  test "aborts when state is different than unset" do
    conn = process(AbortingCallbacks, :GET, "/foo")
    assert conn.status == 302
  end
end

defmodule Dynamo.Router.FinishCallbacksTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule SingleCallbacks do
    use Dynamo.Router

    finalize :foo

    get "/foo" do
      conn.assign(:value, 3).resp(200, "OK")
    end

    defp foo(conn) do
      conn.assign(:value, conn.assigns[:value] * 2)
    end
  end

  test "dispatch single callback" do
    conn = process(SingleCallbacks, :GET, "/foo")
    assert conn.assigns[:value] == 6
  end

  defmodule Bar do
    def service(conn) do
      conn.assign(:value, conn.assigns[:value] + 1)
    end

    def service(conn, { __MODULE__, :data }) do
      conn.assign(:value, conn.assigns[:value] * 2)
    end
  end

  defmodule DoubleCallbacks do
    use Dynamo.Router

    finalize Bar
    finalize { Bar, :data }

    get "/foo" do
      conn.assign(:value, 2).resp(200, "OK")
    end
  end

  test "dispatch double callback" do
    conn = process(SingleCallbacks, :GET, "/foo")
    assert conn.assigns[:value] == 6
  end

  defmodule BlockCallbacks do
    use Dynamo.Router

    finalize do
      conn.assign(:value, conn.assigns[:value] + 1)
    end

    finalize do
      conn.assign(:value, conn.assigns[:value] * 2)
    end

    get "/foo" do
      conn.assign(:value, 2).resp(200, "OK")
    end
  end

  test "dispatch block callback" do
    conn = process(SingleCallbacks, :GET, "/foo")
    assert conn.assigns[:value] == 6
  end

  defmodule InvalidCallbacks do
    use Dynamo.Router

    finalize do
      :ok
    end

    get "/foo" do
      conn.assign(:value, 2)
    end
  end

  test "invalid dispatch callback" do
    assert_raise Dynamo.Router.Callbacks.InvalidCallbackError, fn ->
      process(InvalidCallbacks, :GET, "/foo")
    end
  end
end