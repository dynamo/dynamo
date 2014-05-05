defmodule Dynamo.Connection.QueryParser do
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
  def parse(query, dict \\ Binary.Dict.new)

  def parse("", dict) do
    dict
  end

  def parse(query, dict) do
    decoder = URI.query_decoder(query)
    Enum.reduce(Enum.reverse(decoder), dict, &reduce(&1, &2))
  end

  @doc """
  Receives a raw key, its value and the current accumulator
  and parses the key merging it into the current accumulator.

  Parameters lists are added to the accumulator in reverse order,
  so be sure to pass the parameters in reverse order.
  """
  def reduce({ key, value }, acc) do
    parts =
      # Any path containing [ (except at the initial
      # position) and finishing with ]
      #
      #     users[address][street] #=> [ _, "users", "address][street" ]
      #
      case Regex.run(~r"^([^\[]+)\[(.*)\]$", key) do
        [_all, key, subpart] ->
          [key|String.split(subpart, "][", trim: false)]
        _ ->
          [key]
      end

    assign_parts parts, acc, value
  end

  # We always assign the value in the last segment.
  # `age=17` would match here.
  defp assign_parts([key], acc, value) do
    Binary.Dict.update(acc, key, value, fn
      x when is_list(x) or is_record(x, Binary.Dict) ->
        raise ParseError, message: "expected string at #{key}"
      x -> x
    end)
  end

  # The current segment is a list. We simply prepend
  # the item to the list or create a new one if it does
  # not yet. This assumes that items are iterated in
  # reverse order.
  defp assign_parts([key,""|t], acc, value) do
    current =
      case Binary.Dict.get(acc, key, []) do
        current when is_list(current) -> current
        _   -> raise ParseError, message: "expected list at #{key}"
      end

    if value = assign_list_parts(t, value) do
      Binary.Dict.put(acc, key, [value|current])
    else
      Binary.Dict.put(acc, key, current)
    end
  end

  # The current segment is a parent segment of a
  # dict. We need to create a dictionary and then
  # continue looping.
  defp assign_parts([key|t], acc, value) do
    child =
      case Binary.Dict.get(acc, key) do
        current when is_record(current, Binary.Dict) -> current
        nil -> Binary.Dict.new
        _   -> raise ParseError, message: "expected dict at #{key}"
      end

    value = assign_parts(t, child, value)
    Binary.Dict.put(acc, key, value)
  end

  defp assign_list_parts([], value), do: value
  defp assign_list_parts(t, value),  do: assign_parts(t, Binary.Dict.new, value)
end
