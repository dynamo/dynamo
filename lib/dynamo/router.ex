defmodule Dynamo.Router do
  @moduledoc """
  `Dynamo.Routes` brings routing semantics to your module.

  A Dynamo application is made of many routers that redirect requests
  to specific endpoints.

  Here is a minimal router:

      defmodule HomeRouter do
        use Dynamo.Router

        get "/hello" do
          conn.resp 200, "world"
        end
      end

  This simple router can handle the route "/hello". You can define other
  routes using verbs `get`, `post`, `put` and `delete`. All routes defined
  using such verbs have direct access to the connection (`conn`).

  ## Forwarding

  A Dynamo router can also forward a specific subroute to any other router,
  allowing a developer to scope its application instead of having a big,
  monolitic, routes handler:

      defmodule ApplicationRouter do
        use Dynamo.Router
        forward "/home", to: HomeRouter
      end

  Now any request starting with "/home" in `ApplicationRouter` router will
  be forwarded to `HomeRouter`, but without the "/home" prefix. Therefore
  a request to "/home/hello" is seen by the `HomeRouter` simply as "/hello",
  matching the route we defined previously and returning "world".

  Although in the example above we forwarded to another Dynamo router, we
  can forward to any module, as long as it exports the function `service/1`.
  This function receives the connection as argument and must return the
  updated response.

  The macros for routes definition are imported from `Dynamo.Router.Base`.

  ## Filters

  Routers also support filters, as `Dynamo.App`. For more information about
  filters, check `Dynamo.App` and `Dynamo.Router.Filters` docs.

  ## Callbacks

  Routers also include callbacks functionality via both `prepare/1` and
  `finalize/1` macros. Such callbacks receive the connection as argument
  and may return an updated version. For example:

      defmodule UsersRouter do
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

  ## Fetch

  In Dynamo, parts of the request are parsed lazily. For instance,
  to use `conn.params`, you first need to explicitly fetch the params
  using `conn.fetch(:params)`. Since fetching a specific part of the
  response is common, Dynamo provides a macro to do it:

      defmodule UsersRouter do
        use Dynamo.Router
        fetch [:params, :cookies]
      end

  """

  @doc false
  defmacro __using__(_) do
    quote do
      @dynamo_router true

      if @dynamo_app do
        raise "use Dynamo.App after Dynamo.Router"
      end

      use Dynamo.Utils.Once

      use_once Dynamo.Router.Base
      use_once Dynamo.Router.Callbacks
      use_once Dynamo.Router.Filters
      use_once Dynamo.Router.Fetch
    end
  end
end
