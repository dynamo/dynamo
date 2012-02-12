defmodule Dynamo::Router do

  # Generates a representation that will only match routes according to the
  # given `spec`.
  #
  # ## Examples
  #
  #     generate_match("/foo/:id") => ['foo', { :id, 0, :quoted }]
  #
  def generate_match(spec) do
    generate_match split(spec), []
  end

  # Splits the given path into several segments.
  # It ignores both leading and trailing slashes in the path.
  #
  # ## Examples
  #
  #     split("/foo/bar") #=> ['foo', 'bar']
  #
  def split([?/|t]) do
    split t
  end

  def split(t) do
    split t, [], []
  end

  ## Helpers

  defp generate_match([h|t], acc) do
    generate_match(t, [ dynamic_match(h, []) | acc])
  end

  defp generate_match([], acc) do
    List.reverse acc
  end

  defp dynamic_match([?:|argument], []) do
    { list_to_atom(argument), 0, :quoted }
  end

  defp dynamic_match([?:|argument], buffer) do
    var = { list_to_atom(argument), 0, :quoted }
    { :++, 0, [List.reverse(buffer), var] }
  end

  defp dynamic_match([h|t], buffer) do
    dynamic_match t, [h|buffer]
  end

  defp dynamic_match([], buffer) do
    List.reverse buffer
  end

  defp split(list, buffer, acc) when list == [] orelse list == [?/] do
    List.reverse [List.reverse(buffer)|acc]
  end

  defp split([?/|t], buffer, acc) do
    split t, [], [List.reverse(buffer)|acc]
  end

  defp split([h|t], buffer, acc) do
    split t, [h|buffer], acc
  end
end
