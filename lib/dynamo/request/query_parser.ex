defmodule Dynamo.Request.QueryParser do
  defexception ParseError, message: nil

  @moduledoc """
  Conveniences for parsing query strings in Dynamo.

  Dynamo allows a developer to build query strings
  that maps to Elixir structures in order to make
  manipulation of such structures easier on the server
  side. Here are some examples:

      parse("foo=bar")["foo"] #=> "bar"

  If a value is given more than once, it is overridden:

      parse("foo=bar&foo=baz")["foo"] #=> "baz"

  Nested structures can be created via `[key]`:

      parse("foo[bar]=baz")["foo"]["bar"] #=> "baz"

  Lists are created with `[]`:

      parse("foo[]=bar&foo[]=baz")["foo"] #=> ["baz", "baz"]

  """

  @doc """
  Parses a raw query string, decodes it and returns
  a `Binary.Dict` containing nested hashes.
  """
  def parse("") do
    Binary.Dict.new
  end

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
    Dict.update(acc, key, value, function do
      x when is_list(x) or is_record(x, Binary.Dict) ->
        raise ParseError, message: "expected string at #{key}"
      x -> x
    end)
  end

  defp put_value_on_parts([key,""|t], acc, value) do
    case Dict.get(acc, key, []) do
      current when is_list(current) -> current
      _   -> raise ParseError, message: "expected list at #{key}"
    end

    if value = put_value_on_parts(t, Binary.Dict.new, value) do
      Dict.put(acc, key, [value|current])
    else
      Dict.put(acc, key, current)
    end
  end

  defp put_value_on_parts([key|t], acc, value) do
    child =
      case Dict.get(acc, key) do
        current when is_record(current, Binary.Dict) -> current
        nil -> Binary.Dict.new
        _   -> raise ParseError, message: "expected dict at #{key}"
      end

    value = put_value_on_parts(t, child, value)
    Dict.put(acc, key, value)
  end

  defp put_value_on_parts([], _, value) do
    value
  end
end