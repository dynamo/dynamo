defmodule Dynamo.Request.QueryParser do
  @moduledoc """
  Conveniences for parsing query strings and
  the Dynamo query API.
  """

  @doc """
  Parses a raw query string, decodes it and returns
  a `Binary.Dict` containing nested hashes.
  """
  def parse(query) do
    decoder = URI.query_decoder(query)
    Enum.reduce(Enum.reverse(decoder), Binary.Dict.new, reduce(&1, &2))
  end

  @doc """
  Similar to `reduce(key, value, acc)`.
  """
  def reduce({ key, value }, acc) do
    reduce(key, value, acc)
  end

  @doc """
  Receives a raw key, its value and the current accumulator
  and parses the key merging it into the current accumulator.

  List are added to the accumulator in reverse order, use
  `reverse/1` to loop
  """
  def reduce(key, value, acc) do
    parts =
      case Regex.run(%r"^([^\[]+)\[(.*)\]$", key) do
        [_all, key, subpart] ->
          [key|Binary.split(subpart, "][", global: true)]
        _ ->
          [key]
      end

    put_value_on_parts parts, acc, value
  end

  defp put_value_on_parts([key], acc, value) do
    Dict.update(acc, key, value, identity(&1))
  end

  defp put_value_on_parts([key,""|t], acc, value) do
    case Dict.get(acc, key, []) do
      current when is_list(current) -> current
    end

    value = put_value_on_parts(t, Binary.Dict.new, value)
    Dict.put(acc, key, [value|current])
  end

  defp put_value_on_parts([key|t], acc, value) do
    child =
      case Dict.get(acc, key) do
        current when is_record(current, Binary.Dict) -> current
        nil                                          -> Binary.Dict.new
      end

    value = put_value_on_parts(t, child, value)
    Dict.put(acc, key, value)
  end

  defp put_value_on_parts([], _, value) do
    value
  end

  defp identity(v), do: v
end