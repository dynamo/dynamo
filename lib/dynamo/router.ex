defmodule Dynamo::Router do
  refer Dynamo::Router::GTG, as: GTG
  refer Dynamo::Router::Parser, as: Parser
  refer Dynamo::Router::Compiler, as: Compiler

  def compile(module, routes) do
    gtg = Enum.foldl routes, [], fn({ route, endpoint }, acc) {
      branch = GTG.branch Parser.parse(route), endpoint
      GTG.merge(acc, branch)
    }

    contents = quote do
      def recognize_route(verb, path, dict // 0), do: _0_recognize(verb, path, dict)
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
    Compiler.compile module, gtg, 0
  end
end
