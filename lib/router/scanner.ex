defmodule Dynamo::Router::Scanner do
  def scan(string) do
    tokenize(string, :literal, [], [])
  end

  defp tokenize([?/|t], kind, buffer, acc) do
    acc = build_buffer kind, buffer, acc
    tokenize t, :literal, [], [translate_token(?/)|acc]
  end

  defp tokenize([h|t], kind, buffer, acc) do
    tokenize t, kind, [h|buffer], acc
  end

  defp tokenize([], kind, buffer, acc) do
    List.reverse build_buffer(kind, buffer, acc)
  end

  defp build_buffer(_kind, [], acc) do
    acc
  end

  defp build_buffer(kind, buffer, acc) do
    [{kind, List.reverse(buffer)}|acc]
  end

  defp translate_token(?/), do: { :slash, '/' }
end