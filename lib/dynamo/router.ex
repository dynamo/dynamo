defmodule Dynamo::Router do
  refer Dynamo::Router::GTG
  refer Dynamo::Router::Parser
  refer Dynamo::Router::Compiler

  def compile(module, routes) do
    gtg = Enum.foldl routes, [], fn({ route, endpoint }, acc) {
      branch = GTG.branch Parser.parse(route), endpoint
      GTG.merge(acc, branch)
    }

    contents = quote do
      def recognize_route(verb, path, dict // 0) do
        _recognize_route_0(verb, Dynamo::Router::Parser.normalize(path), dict)
      end
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
    Compiler.compile module, gtg, 0
  end
end
