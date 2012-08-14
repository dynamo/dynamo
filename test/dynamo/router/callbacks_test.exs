Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Router.PrepareCallbacksTest do
  use ExUnit.Case, async: true

  defrecord Mock, value: nil, state: :unset

  defmodule SingleCallbacks do
    use Dynamo.Router

    prepare :foo

    get "/foo" do
      conn.value
    end

    defp foo(conn) do
      conn.value(3)
    end
  end

  test "dispatch single callback" do
    assert SingleCallbacks.dispatch(:GET, ["foo"], Mock.new) == 3
  end

  defmodule Bar do
    def prepare(conn) do
      conn.value(3)
    end

    def other(conn) do
      conn.update_value(&1 * 2)
    end
  end

  defmodule DoubleCallbacks do
    use Dynamo.Router

    prepare Bar
    prepare { Bar, :other }

    get "/foo" do
      conn.value
    end
  end

  test "dispatch double callback" do
    assert DoubleCallbacks.dispatch(:GET, ["foo"], Mock.new) == 6
  end

  defmodule BlockCallbacks do
    use Dynamo.Router

    prepare do
      conn.value(3)
    end

    prepare do
      conn.update_value(&1 * 2)
    end

    get "/foo" do
      conn.value
    end
  end

  test "dispatch block callback" do
    assert BlockCallbacks.dispatch(:GET, ["foo"], Mock.new) == 6
  end

  defmodule InvalidCallbacks do
    use Dynamo.Router

    prepare do
      :ok
    end

    get "/foo" do
      conn.value
    end
  end

  test "invalid dispatch callback" do
    assert_raise Dynamo.Router.Callbacks.InvalidCallbackError, fn ->
      InvalidCallbacks.dispatch(:GET, ["foo"], Mock.new)
    end
  end

  defmodule AbortingCallbacks do
    use Dynamo.Router

    prepare do
      conn.state(:set).value(13)
    end

    get "/foo" do
      raise "will never be invoked"
    end
  end

  test "aborts when state is different than unset" do
    assert AbortingCallbacks.dispatch(:GET, ["foo"], Mock.new).value == 13
  end
end

defmodule Dynamo.Router.FinishCallbacksTest do
  use ExUnit.Case, async: true

  defrecord Mock, value: nil

  defmodule SingleCallbacks do
    use Dynamo.Router

    finalize :foo

    get "/foo" do
      conn.value(3)
    end

    defp foo(conn) do
      conn.update_value(&1 * 2)
    end
  end

  test "dispatch single callback" do
    assert SingleCallbacks.dispatch(:GET, ["foo"], Mock.new).value == 6
  end

  defmodule Bar do
    def finalize(conn) do
      conn.update_value(&1 + 1)
    end

    def other(conn) do
      conn.update_value(&1 * 2)
    end
  end

  defmodule DoubleCallbacks do
    use Dynamo.Router

    finalize Bar
    finalize { Bar, :other }

    get "/foo" do
      conn.value(2)
    end
  end

  test "dispatch double callback" do
    assert DoubleCallbacks.dispatch(:GET, ["foo"], Mock.new).value == 6
  end

  defmodule BlockCallbacks do
    use Dynamo.Router

    finalize do
      conn.update_value(&1 + 1)
    end

    finalize do
      conn.update_value(&1 * 2)
    end

    get "/foo" do
      conn.value(2)
    end
  end

  test "dispatch block callback" do
    assert BlockCallbacks.dispatch(:GET, ["foo"], Mock.new).value == 6
  end

  defmodule InvalidCallbacks do
    use Dynamo.Router

    finalize do
      :ok
    end

    get "/foo" do
      conn.value(2)
    end
  end

  test "invalid dispatch callback" do
    assert_raise Dynamo.Router.Callbacks.InvalidCallbackError, fn ->
      InvalidCallbacks.dispatch(:GET, ["foo"], Mock.new)
    end
  end
end

defmodule Dynamo.Router.FetchTest do
  use ExUnit.Case, async: true

  defrecord Mock, value: [], state: :unset do
    def fetch(aspect, record) do
      record.prepend_value([aspect])
    end
  end

  defmodule ValidFetchTest do
    use Dynamo.Router

    fetch [:cookies, :session]

    get "/foo" do
      conn
    end
  end

  test "fetch aspects of connection" do
    assert ValidFetchTest.dispatch(:GET, ["foo"], Mock.new).value == [:session, :cookies]
  end
end
