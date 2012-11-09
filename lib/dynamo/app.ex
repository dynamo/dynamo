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
  everytime we have a request at `/static`:

      config :dynamo,
        static_root:  :myapp,
        static_route: "/static"

  The available `:dynamo` configurations are:

  * `:static_route` - The route to serve static assets
  * `:static_root` - The location static assets are defined
  * `:compile_on_demand` - Compiles modules as they are needed
  * `:reload_modules` - Reload modules after they are changed
  * `:source_paths` - The paths to search when compiling modules on demand
  * `:view_paths` - The paths to find views
  * `:otp_app` - The otp application associated to this app

  ## Filters

  A `Dynamo.App` also contains a set of filters that are meant
  to be used throughout your whole application. Some of these
  filters are added based on your configuration option. The
  filters included by default and when they are included are:

  * `Dynamo.Filters.Static` - when a static_route and static_root are set,
     we add this filter to serve static assets;
  * `Dynamo.Filters.Head` - converts HEAD requests to GET;
  * `Dynamo.Reloader.Filter` - when `:compile_on_demand` or `:reload_modules`
    configs are set to true, allowing code to be compiled and reloaded on demand;

  Filters can be added and removed using `filter` and `remove_filter`
  macros. You can get the list of application filters using:
  `mix dynamo.filters`.

  For more information, check `Dynamo.Router.Filters` docs.

  ## Initialization

  `Dynamo.App` allows you to register initializers which are
  invoked when the application starts. A Dynamo application
  is initialized in three steps:

  * The dynamo framework needs to be loaded via Dynamo.start
  * The application needs to be loaded via APP.start
  * A handler needs to be run to serve an application

  The step 2 can be extended via initializers. For example:

      defmodule MyApp do
        use Dynamo.App

        initializer :some_config do
          # Connect to the database
        end
      end

  By default, the application ships with 3 initializers:

  * `:start_dynamo_reloader` - starts the code reloader, usually
    used in development and test

  * `:start_dynamo_app` - starts the Dynamo application registered as `otp_app`

  * `:start_dynamo_renderer` - starts dynamo renderer if there
    are templates to be compiled on demand

  """

  @doc false
  defmacro __using__(_) do
    quote do
      require Dynamo.App
      @dynamo_app true

      @before_compile { unquote(__MODULE__), :load_env_file }
      @before_compile { unquote(__MODULE__), :normalize_options }
      @before_compile { unquote(__MODULE__), :define_filters }
      @before_compile { unquote(__MODULE__), :define_view_paths }

      use Dynamo.Utils.Once

      use_once Dynamo.App.Base
      use_once Dynamo.Router.Filters

      filter Dynamo.Filters.Head

      config :dynamo, Dynamo.App.default_options(__ENV__)
      Dynamo.App.default_initializers
    end
  end

  @doc false
  def default_options(env) do
    [ static_route: "/static",
      compile_on_demand: true,
      reload_modules: false,
      source_paths: ["app/*"],
      view_paths: ["app/views"],
      compiled_view_paths: env.module.CompiledViews ]
  end

  @doc false
  defmacro default_initializers do
    quote location: :keep do
      initializer :start_dynamo_reloader do
        dynamo = config[:dynamo]
        if dynamo[:compile_on_demand] do
          Dynamo.Reloader.append_paths dynamo[:source_paths]
          Dynamo.Reloader.enable!
          IEx.preload.after_spawn(fn -> Dynamo.Reloader.enable! end)
        end
      end
    end
  end

  @doc false
  defmacro normalize_options(mod) do
    dynamo = Module.get_attribute(mod, :config)[:dynamo]
    root   = File.cwd!

    source = dynamo[:source_paths]
    source = Enum.reduce source, [], fn(path, acc) -> expand_paths(path, root) ++ acc end

    view = dynamo[:view_paths]
    view = Enum.reduce view, [], fn(path, acc) -> expand_paths(path, root) ++ acc end

    # Remove views that eventually end up on source
    source = source -- view

    # Now convert all view paths to Dynamo.View.Finders
    view = lc path inlist view do
      if is_binary(path) do
        Dynamo.View.PathFinder.new(path)
      else
        path
      end
    end

    quote do
      config :dynamo,
        view_paths: unquote(view),
        source_paths: unquote(source)
    end
  end

  defp expand_paths(path, root) do
    path /> File.expand_path(root) /> File.wildcard
  end

  @doc false
  defmacro load_env_file(_) do
    root = File.rootname(__CALLER__.file, ".ex")
    if File.dir?("#{root}/environments") do
      file = "#{root}/environments/#{Dynamo.env}.exs"
      Code.string_to_ast! File.read!(file), file: file
    end
  end

  @doc false
  defmacro define_filters(_) do
    quote location: :keep do
      Enum.each Dynamo.App.default_filters(__MODULE__), prepend_filter(&1)
      def :filters, [], [], do: Macro.escape(Enum.reverse(@__filters))
    end
  end

  @doc false
  def default_filters(mod) do
    filters = []
    dynamo  = Module.get_attribute(mod, :config)[:dynamo]

    static_route = dynamo[:static_route]
    static_root  = case dynamo[:static_root] do
      nil   -> dynamo[:otp_app]
      other -> other
    end

    if static_root && static_route do
      static  = Dynamo.Filters.Static.new(static_route, static_root)
      filters = [static|filters]
    end

    if dynamo[:compile_on_demand] || dynamo[:reload_modules] do
      reloader = Dynamo.Reloader.Filter.new(dynamo[:compile_on_demand], dynamo[:reload_modules])
      filters  = [reloader|filters]
    end

    if dynamo[:reload_modules] && !dynamo[:compile_on_demand] do
      raise "Cannot have reload_modules set to true and compile_on_demand set to false"
    end

    filters
  end

  @doc false
  defmacro define_view_paths(module) do
    dynamo     = Module.get_attribute(module, :config)[:dynamo]
    view_paths = dynamo[:view_paths]

    { compiled, runtime } =
      if dynamo[:compile_on_demand] do
        { [], view_paths }
      else
        Enum.partition(view_paths, fn(path) -> path.eager? end)
      end

    if compiled != [] do
      module     = dynamo[:compiled_view_paths]
      view_paths = [module|runtime]
    end

    renderer_initializer =
      if runtime != [] do
        quote location: :keep do
          initializer :start_dynamo_renderer do
            Dynamo.View.Renderer.start_link

            if config[:dynamo][:compile_on_demand] do
              Dynamo.Reloader.on_purge(fn -> Dynamo.View.Renderer.clear end)
            end
          end
        end
      end

    quote location: :keep do
      def view_paths, do: unquote(Macro.escape(view_paths))
      unquote(renderer_initializer)
    end
  end
end