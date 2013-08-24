defmodule Dynamo.Utils.MessageVerifier do
  @moduledoc """
  `MessageVerifier` makes it easy to generate and verify messages
  which are signed to prevent tampering.

  For example, the session store uses this verifier to send data
  to the client. Although the data can be read by the client, he
  cannot tamper it.
  """

  @doc """
  Decodes and verifies the encoded binary was not tampared with.
  """
  def verify(secret, encoded) do
    case :binary.split(encoded, "--") do
      [content, digest] when content != "" and digest != "" ->
        if secure_compare(digest(secret, content), digest) do
          { :ok, content |> :base64.decode |> binary_to_term }
        else
          :error
        end
      _ ->
        :error
    end
  end

  @doc """
  Generates an encoded and signed binary for the given term.
  """
  def generate(secret, term) do
    encoded = term |> term_to_binary |> :base64.encode
    encoded <> "--" <> digest(secret, encoded)
  end

  defp digest(secret, data) do
    <<mac :: [integer, size(160)]>> = :crypto.hmac(:sha, secret, data)
    :erlang.integer_to_list(mac, 16) |> iolist_to_binary
  end

  @doc """
  Comapres the two binaries completely, byte by byte,
  to avoid timing attacks.
  """
  def secure_compare(left, right) do
    if size(left) == size(right) do
      compare_each(left, right, true)
    else
      false
    end
  end

  defp compare_each(<<h, left :: binary>>, <<h, right :: binary>>, acc) do
    compare_each(left, right, acc)
  end

  defp compare_each(<<_, left :: binary>>, <<_, right :: binary>>, _acc) do
    compare_each(left, right, false)
  end

  defp compare_each(<<>>, <<>>, acc) do
    acc
  end
end

