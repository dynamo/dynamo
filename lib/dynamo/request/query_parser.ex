defmodule Dynamo.Request.QueryParser do
  def parse(params) do
    parse(params, Binary.Dict.new)
  end

  defp parse([{ key, value }|t], acc) do
    acc =
      case Regex.run(%r"^([^\[]+)\[(.*)\]$", key) do
        [_all, key, subpart] ->
          parts = Binary.split(subpart, "][", global: true)
          put_value_on_parts [key|parts], acc, value
        other ->
          Dict.put(acc, key, value)
      end

    parse(t, acc)
  end

  defp parse([], acc) do
    acc
  end

  defp put_value_on_parts([key|t], acc, value) do
    child =
      case Dict.get(acc, key, :undefined) do
        :undefined -> Binary.Dict.new
        current    -> current # TODO: Validate kind
      end

    value = put_value_on_parts(t, child, value)
    Dict.put(acc, key, value)
  end

  defp put_value_on_parts([], _, value) do
    value
  end
end