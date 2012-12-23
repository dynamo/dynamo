Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.PrepareHooksTest do
  use ExUnit.Case, async: true
  import Dynamo.HTTP.Case, only: [process: 3]

  defmodule SingleHooks do
    use Dynamo.Router

    prepare :foo

    get "/foo" do
      conn.resp_body("OK")
    end

    defp foo(conn) do
      conn.assign(:value, 3)
    end
  end

  test "dispatch single hook" do
    conn = process(SingleHooks, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200
  end

  defmodule Bar do
    def new do
      # We are returning a tuple with three elements
      # to ensure we are escaping the tuple contents
      # with Macro.escape when generating the hook
      { __MODULE__, 1, [] }
    end

    def service(conn) do
      conn.assign(:value, 3)
    end

    def service(conn, { __MODULE__, 1, [] }) do
      conn.assign(:value, conn.assigns[:value] * 2)
    end
  end

  defmodule DoubleHooks do
    use Dynamo.Router

    prepare Bar
    prepare Bar.new

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  test "dispatch double hook" do
    conn = process(DoubleHooks, :GET, "/foo")
    assert conn.assigns[:value] == 6
    assert conn.status == 200
  end

  defmodule BlockHooks do
    use Dynamo.Router

    prepare do
      conn.assign(:value, 3)
    end

    prepare do
      conn.assign(:value, conn.assigns[:value] * 2)
    end

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  test "dispatch block hook" do
    conn = process(DoubleHooks, :GET, "/foo")
    assert conn.assigns[:value] == 6
    assert conn.status == 200
  end

  defmodule InvalidHooks do
    use Dynamo.Router

    prepare do
      :ok
    end

    get "/foo" do
      conn.resp_body("OK")
    end
  end

  test "invalid dispatch hook" do
    assert_raise Dynamo.Router.InvalidHookError, fn ->
      process(InvalidHooks, :GET, "/foo")
    end
  end
end

defmodule Dynamo.Router.FinishHooksTest do
  use ExUnit.Case, async: true
  import Dynamo.HTTP.Case, only: [process: 3]

  defmodule SingleHooks do
    use Dynamo.Router

    finalize :foo

    get "/foo" do
      conn.assign(:value, 3).resp_body("OK")
    end

    defp foo(conn) do
      conn.assign(:value, conn.assigns[:value] * 2)
    end
  end

  test "dispatch single hook" do
    conn = process(SingleHooks, :GET, "/foo")
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

  defmodule DoubleHooks do
    use Dynamo.Router

    finalize Bar
    finalize { Bar, :data }

    get "/foo" do
      conn.assign(:value, 2).resp_body("OK")
    end
  end

  test "dispatch double hook" do
    conn = process(SingleHooks, :GET, "/foo")
    assert conn.assigns[:value] == 6
  end

  defmodule BlockHooks do
    use Dynamo.Router

    finalize do
      conn.assign(:value, conn.assigns[:value] + 1)
    end

    finalize do
      conn.assign(:value, conn.assigns[:value] * 2)
    end

    get "/foo" do
      conn.assign(:value, 2).resp_body("OK")
    end
  end

  test "dispatch block hook" do
    conn = process(SingleHooks, :GET, "/foo")
    assert conn.assigns[:value] == 6
  end

  defmodule InvalidHooks do
    use Dynamo.Router

    finalize do
      :ok
    end

    get "/foo" do
      conn.assign(:value, 2)
    end
  end

  test "invalid dispatch hook" do
    assert_raise Dynamo.Router.InvalidHookError, fn ->
      process(InvalidHooks, :GET, "/foo")
    end
  end
end