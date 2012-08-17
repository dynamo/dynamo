Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Test.ConnectionTest do
  use ExUnit.Case, async: true

  alias Dynamo.Test.Connection, as: C

  def service(conn) do
    function = binary_to_atom hd(conn.path_segments), :utf8
    apply __MODULE__, function, [conn]
  end

  def ok(_conn) do
    :ok
  end

  test :path_segments do
    assert request(:GET, "/ok") == :ok
  end

  defp request(verb, path) do
    service C.new.req(verb, path)
  end
end