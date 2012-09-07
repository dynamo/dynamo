Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.View.HandlerTest do
  use ExUnit.Case, async: true

  test "gets a handler by extension" do
    assert Dynamo.View.Handler.get!("eex") == Dynamo.View.EEXHandler
  end

  test "raises on invalid handler" do
    assert_raise RuntimeError, fn ->
      Dynamo.View.Handler.get!("unknown")
    end
  end
end