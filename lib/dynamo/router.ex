defmodule Dynamo.Router do
  @moduledoc """
  `Dynamo.Routes` brings routing semantics to your module.

  A Dynamo application is made of many routers that redirect requests
  to specific endpoints.

  Here is a minimal router:

      defmodule MyApp.Home do
        use Dynamo.Router

        get "hello" do
          conn.reply 200, "world"
        end
      end

  In this case, the endpoint can handle the route "hello". The verbs `get`,
  `post`, `put` and `delete` are supported.

  ## Forwarding

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
  can forward to any module, as long as it exports the function `service/1`.
  This function receives the request and response as arguments and must
  return the updated response.

  The macros for routes definition are imported from `Dynamo.Router.Base`.

  ## Callbacks

  Routers also include callbacks functionality via both `prepare/1` and
  `finalize/1` macros. Such callbacks receive the connection as argument
  and may return an updated version. For example:

      defmodule MyApp do
        use Dynamo.Router

        prepare :check_user_cookie

        get ...

        defp check_user_cookie(conn) do
          unless conn.cookies[:user_id] do
            redirect_to conn, "/"
          end
        end
      end

  Notice that, if a prepare callbacks replies, redirects or anything,
  the stack aborts and the connection is returned. Check
  `Dynamo.Router.Callbacks` for more information.
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      use Dynamo.Router.Base
      use Dynamo.Router.Callbacks
    end
  end
end
