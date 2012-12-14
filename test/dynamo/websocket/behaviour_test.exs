Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Websocket.BehaviourTest do
  use ExUnit.Case, async: true
  use Dynamo.Websocket.Behaviour

  test :websocket_behaviour do
    assert @behaviour == [Dynamo.Websocket]
  end
end