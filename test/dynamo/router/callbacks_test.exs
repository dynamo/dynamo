Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Router.PrepareCallbacksTest do
  use ExUnit.Case, async: true

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

  defmodule BlockCallbacks do
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
    assert BlockCallbacks.dispatch(:GET, ["foo"], Mock.new, Mock.new) == 6
  end

  defmodule InvalidCallbacks do
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
      InvalidCallbacks.dispatch(:GET, ["foo"], Mock.new, Mock.new)
    end
  end
end

defmodule Dynamo.Router.FinishCallbacksTest do
  use ExUnit.Case, async: true

  defrecord Mock, value: nil

  defmodule SingleCallbacks do
    use Dynamo.Router

    finish :foo

    get "/foo" do
      res.value(2)
    end

    defp foo(req, res) do
      req.value + res.value
    end
  end

  test "dispatch single callback" do
    assert SingleCallbacks.dispatch(:GET, ["foo"], Mock.new(value: 1), Mock.new) == 3
  end

  defmodule Bar do
    def finish(_req, res) do
      res.update_value(&1 + 1)
    end

    def other(req, res) do
      req.value + res.value
    end
  end

  defmodule DoubleCallbacks do
    use Dynamo.Router

    finish Bar
    finish { Bar, :other }

    get "/foo" do
      res.value(2)
    end
  end

  test "dispatch double callback" do
    assert DoubleCallbacks.dispatch(:GET, ["foo"], Mock.new(value: 1), Mock.new) == 4
  end
end
