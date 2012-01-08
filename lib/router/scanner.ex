defmodule Dynamo::Router::Scanner do
  def scan(string) do
    tokenize(string, [], [])
  end

  defp tokenize([?/|t], buffer, acc) do
    acc = build_buffer buffer, acc
    tokenize t, [], [translate_token(?/)|acc]
  end

  defp tokenize([h|t], buffer, acc) do
    tokenize t, [h|buffer], acc
  end

  defp tokenize([], buffer, acc) do
    List.reverse build_buffer(buffer, acc)
  end

  defp build_buffer([], acc) do
    acc
  end

  defp build_buffer(buffer, acc) do
    [{:literal, List.reverse(buffer)}|acc]
  end

  defp translate_token(?/), do: { :slash, '/' }
end