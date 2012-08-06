defmodule Dynamo.Router do
  @moduledoc """
  `Dynamo.Routes` brings routing semantics to your module.

  A Dynamo application is made of many routers that redirects requests
  to specific endpoints.

  Here is a minimal router:

      defmodule MyApp.Home do
        use Dynamo.Router

        get "hello" do
          close response, "world"
        end
      end

  In this case, the endpoint can handle the route "hello". The verbs `get`,
  `post`, `put` and `delete` are supported.

  A Dynamo router can also forward a specific subroute to any other router,
  allowing a developer to scope its application instead of having a big,
  monolitic, routes handler:

      defmodule MyApp.Main do
        use Dynamo.Router
        forward "home", to: MyApp.Home
      end

  Now any request at "home" in the `MyApp.Main` router will be forwarded
  to `MyApp.Home`, but without the "home" prefix. So a request at "home/hello"
  is seen by the `MyApp.Home` simply as "hello", matching the route we
  defined previously and returning "world".

  Although in the example above we forwarded to another Dynamo router, we
  can forward to any module, as long as it exports the function `service/2`.
  This function receives the request and response as arguments and must
  return the updated response.

  The macros for routes definition are imported from `Dynamo.Router.DSL`.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @before_compile unquote(__MODULE__)
      import Dynamo.Router.DSL

      def service(req, res) do
        dispatch(req.method, req.path_segments, req, res)
      end

      def not_found(_req, res) do
        res.reply(404, [], "")
      end

      def dynamo_router? do
        true
      end

      defoverridable [not_found: 2, service: 2]
    end
  end

  @doc false
  defmacro before_compile(_) do
    quote do
      def dispatch(_, _, request, response) do
        not_found(request, response)
      end
    end
  end
end
