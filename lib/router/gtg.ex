defmodule Dynamo::Router::GTG do
  # Receives a tree produced by Dynamo::Router::Parser
  # and converts it to a branch of a generalized
  # transition graph.
  def branch(ast, endpoint) do
    visit(ast, endpoint)
  end

  defp visit({ :cat, left, right }, endpoint) do
    left  = terminal(left)
    right = visit(right, endpoint)
    { left, [right] }
  end

  defp visit(tuple, endpoint) do
    { terminal(tuple), [{ :endpoint, endpoint }] }
  end

  # Handle terminals

  defp terminal({ :terminal, contents }) do
    contents
  end
end