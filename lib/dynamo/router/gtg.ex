# This module is responsible for handling generalized graphs.
# Those graphs are list of tuples where the first element
# of the tuple is an identifier and the second one is another
# list of tuples.
defmodule Dynamo::Router::GTG do
  # Receives a tree produced by Dynamo::Router::Parser
  # and converts it to a branch of a generalized
  # transition graph. The branch is made by two parts
  # the left part representing the current node and
  # the right part representing the nodes the left part
  # points to.
  def branch(left, right), do: do_branch(left, right)

  # Merge a new branch to the list of branches.
  def merge(list, new), do: do_merge(list, new)

  ## Helpers related to branch

  # If the node is a cat, the left side is surely
  # a terminal but we need to expand the right side.
  defp do_branch({ :cat, left, right }, endpoint) do
    left  = terminal(left)
    right = do_branch(right, endpoint)
    { left, [right] }
  end

  # We have reached the end of the branch, so we attach
  # the endpoint.
  defp do_branch(tuple, endpoint) do
    { terminal(tuple), [{ :endpoint, endpoint }] }
  end

  defp terminal({ :terminal, contents }) do
    contents
  end

  ## Helpers related to merge

  # If the right branch matches the left one,
  # we proceed merging the sub-branches
  # recursively.
  defp do_merge([{ left, old }|t], { left, new }) do
    do_merge(left, old, new, t)
  end

  # The branches didn't match, keep searching.
  defp do_merge([h|t], tuple) do
    [h|do_merge(t, tuple)]
  end

  # If we reached the end of the list, we haven't
  # found a match. So add the tuple and done.
  defp do_merge([], tuple) do
    [tuple]
  end

  # Handle merge_each for endpoints where old and new are not lists.
  defp do_merge(:endpoint, old, new, acc) do
    [{ :endpoint, old }, { :endpoint, new }|acc]
  end

  # Merge each item when old/new are lists.
  defp do_merge(left, old, new, acc) do
    merged = Enum.reduce new, old, fn(x, acc, do: do_merge(acc, x))
    [{ left, merged }|acc]
  end
end