defmodule Dynamo.Utils.Incrementer do
  defmacro __using__(_) do
    quote do
      @counter 2
    end
  end
end

defmodule Dynamo.Utils.OnceTest do
  use ExUnit.Case, async: true

  Module.register_attribute __MODULE__, :counter, accumulate: true
  @counter 1

  use Dynamo.Utils.Once
  use_once Dynamo.Utils.Incrementer
  use_once Dynamo.Utils.Incrementer

  use Dynamo.Utils.Once
  use_once Dynamo.Utils.Incrementer
  use_once Dynamo.Utils.Incrementer

  def snapshot do
    @counter
  end

  test "module is used just once" do
    assert snapshot == [2, 1]
  end
end
