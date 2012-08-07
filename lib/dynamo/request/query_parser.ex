defmodule Dynamo.Request.QueryParser do
  def parse(params) do
    parse(params, [])
  end

  defp parse([h|t], acc) do
    { key, value } = h

    acc =
      case :binary.split(key, "[") do
        [_] -> [h|acc]
        [key, subpart] ->
          fullpart = "[" <> subpart
          child = :binary.part(fullpart, 1, size(fullpart) - 2)
          case List.keyfind(acc, key, 1) do
            { _, actual } ->
              :lists.keyreplace(key, 1, acc, { key, [{ child, value }|actual] })
            nil ->
              [{ key, [{ child, value }] }|acc]
          end
      end

    parse(t, acc)
  end

  defp parse([], acc) do
    List.reverse(acc)
  end
end