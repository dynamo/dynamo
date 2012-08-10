Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Router.CallbacksTest do
  use ExUnit.Case

  defrecord Mock, value: nil

  defmodule SingleCallbacks do
    use Dynamo.Router

    prepare :foo

    get "/foo" do
      req.value + res.value
    end

    defp foo(req, res) do
      { req.value(1), res.value(2) }
    end
  end

  test "dispatch single callback" do
    assert SingleCallbacks.dispatch(:GET, ["foo"], Mock.new, Mock.new) == 3
  end

  defmodule Bar do
    def prepare(req, res) do
      { req.value(1), res.value(2) }
    end

    def other(req, res) do
      { req.update_value(&1 + 1), res.update_value(&1 + 2) }
    end
  end

  defmodule DoubleCallbacks do
    use Dynamo.Router

    prepare Bar
    prepare { Bar, :other }

    get "/foo" do
      req.value + res.value
    end
  end

  test "dispatch double callback" do
    assert DoubleCallbacks.dispatch(:GET, ["foo"], Mock.new, Mock.new) == 6
  end

  defmodule PrepareBlockCallbacks do
    use Dynamo.Router

    prepare do
      { req.value(1), res.value(2) }
    end

    prepare do
      { req.update_value(&1 + 1), res.update_value(&1 + 2) }
    end

    get "/foo" do
      req.value + res.value
    end
  end

  test "dispatch block callback" do
    assert PrepareBlockCallbacks.dispatch(:GET, ["foo"], Mock.new, Mock.new) == 6
  end

  defmodule InvalidPrepareCallback do
    use Dynamo.Router

    prepare do
      :ok
    end

    get "/foo" do
      req.value + res.value
    end
  end

  test "invalid dispatch callback" do
    assert_raise Dynamo.Router.Callbacks.InvalidPrepareCallbackError, fn ->
      InvalidPrepareCallback.dispatch(:GET, ["foo"], Mock.new, Mock.new)
    end
  end
end