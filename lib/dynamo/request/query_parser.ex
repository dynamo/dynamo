defmodule Dynamo.Request.QueryParser do
  def parse(params) do
    parse(params, Binary.Dict.new)
  end

  defp parse([{ key, value }|t], acc) do
    acc =
      case Regex.run(%r"^([^\[]+)\[(.*)\]$", key) do
        [_all, key, subpart] ->
          current = Dict.get(acc, key, Binary.Dict.new)
          Dict.put(acc, key, Dict.put(current, subpart, value))
        other ->
          Dict.put(acc, key, value)
      end

    parse(t, acc)
  end

  defp parse([], acc) do
    acc
  end
end