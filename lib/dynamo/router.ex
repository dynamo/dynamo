defmodule Dynamo::Router do
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