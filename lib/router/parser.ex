defmodule Dynamo::Router::Parser do
  def parse(string) do
    tokens = Dynamo::Router::Scanner.scan(normalize(string))
    { :ok, result } = Erlang.dynamo_router_parser.parse(tokens)
    result
  end

  def normalize(string) do
    string
  end
end