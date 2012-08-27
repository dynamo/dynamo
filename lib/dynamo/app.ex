defmodule Dynamo.App do
  @moduledoc """
  `Dynamo.App` is a module that helps you define your
  application behavior globally.

  A `Dynamo.App` can be used on top of a `Dynamo.Router`,
  so you can route and point to other endpoints easily.

  ## Configuration

  Dynamo comes with a configuration API that allows a
  developer to customize how dynamo works and custom
  extensions.

  For example, here is a snippet that configures Dynamo
  to serve public assets from the :myapp application
  everytime we have a request at `/public`:

      config :dynamo,
        public_root:  :myapp,
        public_route: "/public"

  ## Not found

  Each `Dynamo.Router` has a `not_found` hook that is
  invoked whenever a route does not match. However, if
  a developer wants to consistently customize how a 404
  page looks like, he shouldn't need to customize each
  `Dynamo.Router` in his application. That's when
  `Dynamo.App` comes in.

  By default, a `Dynamo.Router` sets the response status
  to 404 whenever a route doesn't match. `Dynamo.App` then
  intercepts all not sent responses with status 404 and
  invokes its own `not_found` function, which can then be
  customized by the developer, for example:

      def not_found(conn) do
        html conn, "404.eex"
      end

  """

  @doc false
  defmacro __using__(_) do
    quote do
      @on_load :register_dynamo_app
      @dynamo_app true
      @before_compile { unquote(__MODULE__), :load_env }

      use Dynamo.Utils.Once

      use_once Dynamo.App.Config
      use_once Dynamo.App.NotFound
      use_once Dynamo.Router.Filters

      @before_compile { unquote(__MODULE__), :apply_filters }

      config :dynamo,
        public_route: "/public",
        translate_head_to_get: true,
        compile_on_demand: false,
        reload_modules: false

      defp register_dynamo_app do
        Dynamo.app(__MODULE__)
      end
    end
  end

  @doc false
  defmacro load_env(module) do
    root = Module.read_attribute(module, :root)
    if root && File.dir?("#{root}/config/environments") do
      file = "#{root}/config/environments/#{Dynamo.env}.ex"
      Code.string_to_ast! File.read!(file), file: file
    end
  end

  @doc false
  defmacro apply_filters(_) do
    quote do
      dynamo = @config[:dynamo]

      if root = dynamo[:public_root] do
        filter Dynamo.Filters.Static.new(dynamo[:public_route], root)
      end

      if dynamo[:translate_head_to_get] do
        filter Dynamo.Filters.Head
      end
    end
  end
end