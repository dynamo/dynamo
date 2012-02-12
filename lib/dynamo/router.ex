defmodule Dynamo::Router do
  require Dynamo::Router::GTG
  require Dynamo::Router::Parser
  require Dynamo::Router::Compiler

  def compile(module, routes) do
    gtg = Enum.reduce routes, [], fn({ route, endpoint }, acc) ->
      branch = GTG.branch Parser.parse(route), endpoint
      GTG.merge(acc, branch)
    end

    contents = quote do
      def recognize_route(verb, path, dict) do
        _recognize_route_0(verb, Dynamo::Router::Parser.normalize(path), dict)
      end
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
    Compiler.compile module, gtg, 0
  end
end
