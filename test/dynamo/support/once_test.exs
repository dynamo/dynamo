Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Support.Incrementer do
  defmacro __using__(_) do
    quote do
      @counter 2
    end
  end
end

defmodule Dynamo.Support.OnceTest do
  use ExUnit.Case

  Module.register_attribute __MODULE__, :counter, accumulate: true
  @counter 1

  use Dynamo.Support.Once
  use_once Dynamo.Support.Incrementer
  use_once Dynamo.Support.Incrementer

  use Dynamo.Support.Once
  use_once Dynamo.Support.Incrementer
  use_once Dynamo.Support.Incrementer

  def snapshot do
    @counter
  end

  test "module is used just once" do
    assert snapshot == [2, 1]
  end
end
