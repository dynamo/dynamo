Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Utils.MessageVerifierTest do
  use ExUnit.Case

  alias Dynamo.Utils.MessageVerifier, as: MV

  test "generates a signed message" do
    [content, encoded] = Binary.split MV.generate(:hello, "secret"), "--"
    assert content /> :base64.decode /> binary_to_term == :hello
    assert size(encoded) == 40
  end

  test "verifies a signed message" do
    signed = MV.generate(:hello, "secret")
    assert MV.verify(signed, "secret") == { :ok, :hello }
  end

  test "does not verify a signed message if secret changed" do
    signed = MV.generate(:hello, "secret")
    assert MV.verify(signed, "secreto") == :error
  end

  test "does not verify a tampered message" do
    [_, encoded] = Binary.split MV.generate(:hello, "secret"), "--"
    content = :bye /> term_to_binary /> :base64.encode
    assert MV.verify(content <> "--" <> encoded, "secret") == :error
  end
end
