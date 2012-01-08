defmodule Dynamo::Router::Parser do
  def parse(string) do
    tokens = Dynamo::Router::Scanner.scan(normalize(string))
    { :ok, result } = Erlang.dynamo_router_parser.parse(tokens)
    result
  end

  def normalize([?/|string]), do: normalize(string)
  def normalize(string),      do: remove_last_slash(string)

  defp remove_last_slash([?/]),  do: []
  defp remove_last_slash([h|t]), do: [h|remove_last_slash(t)]
  defp remove_last_slash([]),    do: []
end