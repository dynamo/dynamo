Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Utils.MessageVerifierTest do
  use ExUnit.Case, async: true

  alias Dynamo.Utils.MessageVerifier, as: MV

  test "generates a signed message" do
    [content, encoded] = String.split MV.generate("secret", :hello), "--"
    assert content |> :base64.decode |> binary_to_term == :hello
    assert size(encoded) == 40
  end

  test "verifies a signed message" do
    signed = MV.generate("secret", :hello)
    assert MV.verify("secret", signed) == { :ok, :hello }
  end

  test "does not verify a signed message if secret changed" do
    signed = MV.generate("secret", :hello)
    assert MV.verify("secreto", signed) == :error
  end

  test "does not verify a tampered message" do
    [_, encoded] = String.split MV.generate("secret", :hello), "--"
    content = :bye |> term_to_binary |> :base64.encode
    assert MV.verify("secret", content <> "--" <> encoded) == :error
  end
end
