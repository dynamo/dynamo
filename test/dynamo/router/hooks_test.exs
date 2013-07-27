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

    def prepare(conn) do
      conn.assign(:value, 4)
    end

    def prepare(conn, { __MODULE__, 1, [] }) do
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
    assert conn.assigns[:value] == 8
    assert conn.status == 200
  end

  defmodule ModFunHooks do
    use Dynamo.Router

    prepare { __MODULE__, :hello }

    def hello(conn) do
      conn.assign(:value, 2)
    end

    get "/foo" do
      conn.assign(:value, conn.assigns[:value] * 2).resp_body("OK")
    end
  end

  test "dispatch mod/fun hook" do
    conn = process(ModFunHooks, :GET, "/foo")
    assert conn.assigns[:value] == 4
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
    conn = process(BlockHooks, :GET, "/foo")
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

  defmodule AnnotatedHooks do
    use Dynamo.Router

    @prepare :foo
    get "/foo" do
      conn.resp_body("OK")
    end

    get "/bar" do
      conn.resp_body("OK")
    end

    defp foo(conn) do
      conn.assign(:value, 3)
    end
  end

  test "dispatch annotated hook" do
    conn = process(AnnotatedHooks, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200

    conn = process(AnnotatedHooks, :GET, "/bar")
    assert conn.assigns[:value] == nil
    assert conn.status == 200
  end

  defmodule SkipHooks do
    use Dynamo.Router

    prepare :assign_one
    prepare :assign_two

    @skip_prepare :assign_one
    get "/foo" do
      conn.resp_body("FOO")
    end

    get "/bar" do
      conn.resp_body("BAR")
    end

    @skip_prepare :assign_two
    get "/baz" do
      conn.resp_body("BAZ")
    end

    @skip_prepare :assign_one
    @skip_prepare :assign_two
    get "/bat" do
      conn.resp_body("BAZ")
    end

    defp assign_one(conn) do
      conn.assign(:one, 1)
    end

    defp assign_two(conn) do
      conn.assign(:two, 2)
    end
  end

  test "skip the desired hooks" do
    conn = process(SkipHooks, :GET, "/foo")
    assert conn.assigns[:one] == nil
    assert conn.assigns[:two] == 2

    conn = process(SkipHooks, :GET, "/bar")
    assert conn.assigns[:one] == 1
    assert conn.assigns[:two] == 2

    conn = process(SkipHooks, :GET, "/baz")
    assert conn.assigns[:one] == 1
    assert conn.assigns[:two] == nil

    conn = process(SkipHooks, :GET, "/bat")
    assert conn.assigns[:one] == nil
    assert conn.assigns[:two] == nil
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
    def finalize(conn) do
      conn.assign(:value, conn.assigns[:value] + 1)
    end

    def finalize(conn, { __MODULE__, :data }) do
      conn.assign(:value, conn.assigns[:value] * 2)
    end
  end

  defmodule DoubleHooks do
    use Dynamo.Router

    finalize Bar
    finalize { Bar, :data }

    get "/foo" do
      conn.assign(:value, 3).resp_body("OK")
    end
  end

  test "dispatch double hook" do
    conn = process(DoubleHooks, :GET, "/foo")
    assert conn.assigns[:value] == 8
  end

  defmodule ModFunHooks do
    use Dynamo.Router

    finalize { __MODULE__, :hello }

    def hello(conn) do
      conn.assign(:value, conn.assigns[:value] * 2)
    end

    get "/foo" do
      conn.assign(:value, 2).resp_body("OK")
    end
  end

  test "dispatch mod/fun hook" do
    conn = process(ModFunHooks, :GET, "/foo")
    assert conn.assigns[:value] == 4
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
      conn.assign(:value, 5).resp_body("OK")
    end
  end

  test "dispatch block hook" do
    conn = process(BlockHooks, :GET, "/foo")
    assert conn.assigns[:value] == 12
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

  defmodule AnnotatedHooks do
    use Dynamo.Router

    @finalize :foo
    get "/foo" do
      conn.resp_body("OK")
    end

    get "/bar" do
      conn.resp_body("OK")
    end

    defp foo(conn) do
      conn.assign(:value, 3)
    end
  end

  test "dispatch annotated hook" do
    conn = process(AnnotatedHooks, :GET, "/foo")
    assert conn.assigns[:value] == 3
    assert conn.status == 200

    conn = process(AnnotatedHooks, :GET, "/bar")
    assert conn.assigns[:value] == nil
    assert conn.status == 200
  end

  defmodule SkipHooks do
    use Dynamo.Router

    finalize :assign_one
    finalize :assign_two

    @skip_finalize :assign_one
    get "/foo" do
      conn.resp_body("FOO")
    end

    get "/bar" do
      conn.resp_body("BAR")
    end

    @skip_finalize :assign_two
    get "/baz" do
      conn.resp_body("BAZ")
    end

    @skip_finalize :assign_one
    @skip_finalize :assign_two
    get "/bat" do
      conn.resp_body("BAZ")
    end

    defp assign_one(conn) do
      conn.assign(:one, 1)
    end

    defp assign_two(conn) do
      conn.assign(:two, 2)
    end
  end

  test "skip the desired hooks" do
    conn = process(SkipHooks, :GET, "/foo")
    assert conn.assigns[:one] == nil
    assert conn.assigns[:two] == 2

    conn = process(SkipHooks, :GET, "/bar")
    assert conn.assigns[:one] == 1
    assert conn.assigns[:two] == 2

    conn = process(SkipHooks, :GET, "/baz")
    assert conn.assigns[:one] == 1
    assert conn.assigns[:two] == nil

    conn = process(SkipHooks, :GET, "/bat")
    assert conn.assigns[:one] == nil
    assert conn.assigns[:two] == nil
  end
end
