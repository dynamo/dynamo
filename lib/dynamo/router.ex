defmodule Dynamo.Router do
  @moduledoc """
  A Dynamo is made of many routers that redirect a connection
  between endpoints. Those endpoints are usually built with
  `Dynamo.Routes` which are responsible for bringing routing
  semantics to your modules.

  Here is a minimal router:

      defmodule HomeRouter do
        use Dynamo.Router

        get "/hello" do
          conn.resp 200, "world"
        end
      end

  The functionality and documentation for routers is split into
  diverse modules. Keep reading below for more information.

  ## Routes and hooks

  A router is made by many routes which usually map to an HTTP
  verb like `get`, `post`, etc. Not only that, a router may easily
  forward requests to another router to handle a specific set of
  requests via the forward API:

      defmodule ApplicationRouter do
        use Dynamo.Router
        forward "/home", to: HomeRouter
      end

  Hooks can be used to prepare and finalize requests after a given
  route match. For more information, check `Dynamo.Router.Base`
  documentation.

  ## Filters

  As a Dynamo, Routers also support filters. For more information about
  filters, check `Dynamo` and `Dynamo.Router.Filters` docs.

  ## HTTP conveniences

  Finally, a router imports by default many of the `Dynamo.HTTP`
  modules, as listed below:

  * `Dynamo.HTTP.Cookies` - conveniences for working with cookies
  * `Dynamo.HTTP.Halt` - conveniences for halting a request
  * `Dynamo.HTTP.Hibernate` - conveniences for awaiting and hibernating a connection
  * `Dynamo.HTTP.Redirect` - conveniences for redirect connection
  * `Dynamo.HTTP.Render` - conveniences for rendering templates
  * `Dynamo.HTTP.Session` - conveniences for working with session

  """

  @doc false
  defmacro __using__(_) do
    quote do
      @dynamo_router true
      use Dynamo.Utils.Once

      use_once Dynamo.Router.Base
      use_once Dynamo.Router.Filters
      use_once Dynamo.HTTP
    end
  end
end
