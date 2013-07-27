defmodule Dynamo.Templates.HandlerTest do
  use ExUnit.Case, async: true

  test "gets a handler by extension" do
    assert Dynamo.Templates.Handler.get!("eex") == Dynamo.Templates.EEXHandler
  end

  test "raises on invalid handler" do
    assert_raise RuntimeError, fn ->
      Dynamo.Templates.Handler.get!("unknown")
    end
  end
end