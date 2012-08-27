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
      @before_compile { unquote(__MODULE__), :apply_filters }
      @before_compile { unquote(__MODULE__), :apply_initializers }

      if :code.is_loaded(__MODULE__) do
        IO.puts "[ERROR] The dynamo application #{inspect __MODULE__} is already loaded. This may " <>
          "happen because there is already a compiled app file at ebin. Please ensure your ebin " <>
          "directory does not contain compiled app files with `mix clean`"
        exit(1)
      end

      use Dynamo.Utils.Once

      use_once Dynamo.App.Config
      use_once Dynamo.App.NotFound
      use_once Dynamo.Router.Filters

      if Enum.any?(:application.loaded_applications, match?({ :mix, _, _ }, &1)) do
        app = Mix.project[:app]
      end

      config :dynamo,
        public_route: "/public",
        translate_head_to_get: true,
        compile_on_demand: false,
        reload_modules: false,
        source_paths: File.wildcard("app/*"),
        view_paths: ["app/views"],
        root: File.expand_path("../..", __FILE__),
        handler: Dynamo.Cowboy,
        app: app

      # The reloader needs to be the first initializer
      initializer :start_dynamo_reloader do
        dynamo = config[:dynamo]
        if dynamo[:compile_on_demand] do
          Dynamo.Reloader.start_link dynamo[:source_paths]
          Dynamo.Reloader.enable!
        end
      end

      # Then starts up the application
      initializer :start_dynamo_app do
        if app = config[:dynamo][:app] do
          :application.start(app)
        end
      end

      defp register_dynamo_app do
        Dynamo.app(__MODULE__)
      end
    end
  end

  @doc false
  defmacro load_env(module) do
    root = Module.read_attribute(module, :config)[:dynamo][:root]
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

      if dynamo[:compile_on_demand] || dynamo[:reload_modules] do
        filter Dynamo.Filters.Reloader.new(dynamo[:compile_on_demand], dynamo[:reload_modules])
      end
    end
  end

  @doc false
  defmacro apply_initializers(_) do
    quote do
      initializer :ensure_endpoint_is_available do
        if @endpoint && match?({ :error, _ }, Code.ensure_compiled(@endpoint)) do
          if config[:dynamo][:compile_on_demand] do
            raise "could not find endpoint #{inspect @endpoint}, please ensure it is available"
          else
            raise "could not find endpoint #{inspect @endpoint}, please ensure it was compiled"
          end
        end
      end
    end
  end
end