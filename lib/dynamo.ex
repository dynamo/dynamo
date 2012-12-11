defmodule Dynamo do
  @moduledoc """
  This module allows developers to configure the Dynamo
  framework and define Dynamos.

  A Dynamo is nothing more than a module with a `start`
  and `run` functions and can be easily defined as follow:

      defmodule MyDynamo do
        use Dynamo
        endpoint SomeRouter
      end

  A Dynamo can be used on top of a `Dynamo.Router` in case
  you want to extend a single router to a single file Dynamo.

  ## Configuration

  A Dynamo comes with a configuration API that allows a
  developer to customize how dynamo works and custom
  extensions.

  For example, here is a snippet that configures Dynamo
  to serve public assets from the :myapp application
  everytime we have a request at `/static`:

      config :dynamo,
        env: "prod",
        endpoint: ApplicationRouter,
        static_root:  :myapp,
        static_route: "/static"

  The available `:dynamo` configurations are:

  * `:env` - The environment this Dynamo runs on
  * `:endpoint` - The endpoint to dispatch requests too
  * `:supervisor` - The supervisor local node name
  * `:static_route` - The route to serve static assets
  * `:static_root` - The location static assets are defined
  * `:compile_on_demand` - Compiles modules as they are needed
  * `:reload_modules` - Reload modules after they are changed
  * `:source_paths` - The paths to search when compiling modules on demand
  * `:templates_paths` - The paths to find templates

  Check `Dynamo.Base` for more information on `config` and
  other initialize configuration.

  ## Filters

  A Dynamo also contains a set of filters that are meant
  to be used throughout your whole application. Some of these
  filters are added based on the configuration options above.
  The filters included by default and when they are included are:

  * `Dynamo.Filters.Static` - when a static_route and static_root are set,
     this filter is added to serve static assets;
  * `Dynamo.Filters.Head` - converts HEAD requests to GET;
  * `Dynamo.Reloader.Filter` - when `:compile_on_demand` or `:reload_modules`
    configs are set to true, this filter is added to compiled and reloaded
    code on demand;

  Filters can be added and removed using `filter` and `remove_filter`
  macros. You can get the list of all dynamos filters using:
  `mix dynamo.filters`.

  For more information, check `Dynamo.Router.Filters` docs.

  ## Initialization

  A Dynamo allows you to register initializers which are
  invoked when the dynamo starts. A Dynamo is initialized
  in three steps:

  * The `:dynamo` application needs to be started
  * All dynamos needs to be loaded via `DYNAMO.start_link`
  * A dynamo is hooked into a web server via `DYNAMO.run`

  The step 2 can be extended via initializers. For example:

      defmodule MyDynamo do
        use Dynamo

        initializer :some_config do
          # Connect to the database
        end
      end

  By default, the application ships with some initializers:

  * `:start_dynamo_reloader` - starts the code reloader, usually
    used in development and test

  * `:start_dynamo_renderer` - starts dynamo renderer if there
    are templates to be compiled on demand

  """

  @doc """
  Gets the Dynamo used by default under test.
  """
  def under_test() do
    { :ok, mod } = :application.get_env(:dynamo, :under_test)
    mod
  end

  @doc """
  Sets the Dynamo to be used under test.
  """
  def under_test(mod) do
    :application.set_env(:dynamo, :under_test, mod)
  end

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @is_dynamo true

      @before_compile { unquote(__MODULE__), :load_env_file }
      @before_compile { unquote(__MODULE__), :normalize_paths }
      @before_compile { unquote(__MODULE__), :define_endpoint }
      @before_compile { unquote(__MODULE__), :define_supervisor }
      @before_compile { unquote(__MODULE__), :define_filters }
      @before_compile { unquote(__MODULE__), :define_templates_paths }

      use Dynamo.Utils.Once

      use_once Dynamo.Base
      use_once Dynamo.Router.Filters

      config :dynamo, unquote(default_dynamo_config(__CALLER__))
      config :server, [handler: Dynamo.Cowboy, port: 4000]

      @doc """
      Starts the application supervisor and run all
      registered initializers.
      """
      def start_link(opts // []) do
        info = Dynamo.Supervisor.start_link(supervisor, opts)
        run_initializers
        info
      end

      @doc """
      Runs the app in the configured web server.
      """
      def run(options // []) do
        options = Keyword.merge(config[:server], options)
        options = Keyword.put(options, :env, config[:dynamo][:env])
        options[:handler].run __MODULE__, options
      end

      initializer :start_dynamo_reloader do
        dynamo = config[:dynamo]

        if dynamo[:compile_on_demand] do
          Dynamo.Reloader.append_paths(dynamo[:source_paths])
          Dynamo.Reloader.enable

          if IEx.started? do
            IEx.after_spawn(fn -> Dynamo.Reloader.enable end)
          end
        end
      end

      initializer :start_dynamo_renderer do
        precompiled = Enum.all?(templates_paths, Dynamo.Templates.Finder.precompiled?(&1))
        unless precompiled do
          Dynamo.Templates.Renderer.start_link

          if config[:dynamo][:compile_on_demand], do:
            Dynamo.Reloader.on_purge(fn -> Dynamo.Templates.Renderer.clear end)
        end
      end
    end
  end

  ## Helpers

  defp default_dynamo_config(env) do
    [ env: "prod",
      static_route: "/static",
      compile_on_demand: true,
      reload_modules: false,
      source_paths: ["app/*"],
      environments_path: File.join(File.rootname(env.file, ".ex"), "environments"),
      templates_paths: ["app/templates"],
      supervisor: env.module.Supervisor,
      compiled_templates: env.module.CompiledTemplates ]
  end

  ## __before_compile__ callbacks

  @doc false
  defmacro load_env_file(_) do
    dynamo = Module.get_attribute(__CALLER__.module, :config)[:dynamo]
    dir = dynamo[:environments_path]
    env = dynamo[:env]
    if dir && File.dir?(dir) do
      file = "#{dir}/#{env}.exs"
      Code.string_to_ast! File.read!(file), file: file
    end
  end

  @doc false
  defmacro normalize_paths(mod) do
    dynamo = Module.get_attribute(mod, :config)[:dynamo]
    root   = File.cwd!

    source = dynamo[:source_paths]
    source = Enum.reduce source, [], fn(path, acc) -> expand_paths(path, root) ++ acc end

    templates = dynamo[:templates_paths]
    templates = Enum.reduce templates, [], fn(path, acc) -> expand_paths(path, root) ++ acc end

    # Remove templates that eventually end up on source
    source = source -- templates

    quote do
      config :dynamo,
        templates_paths: unquote(templates),
        source_paths: unquote(source)
    end
  end

  defp expand_paths(path, root) do
    path /> File.expand_path(root) /> File.wildcard
  end

  @doc false
  defmacro define_filters(_) do
    quote location: :keep do
      Enum.each Dynamo.define_filters(__MODULE__, []), prepend_filter(&1)
    end
  end

  @doc false
  def define_filters(mod, filters) do
    dynamo = Module.get_attribute(mod, :config)[:dynamo]

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

    filters = [Dynamo.Filters.Head|filters]
    filters
  end

  @doc false
  defmacro define_endpoint(module) do
    dynamo = Module.get_attribute(module, :config)[:dynamo]

    quote location: :keep do
      @endpoint unquote(dynamo[:endpoint])

      if @endpoint do
        @doc """
        Receives a connection and dispatches it to #{inspect @endpoint}
        """
        def service(conn) do
          @endpoint.service(conn)
        end
      end

      @doc """
      Returns the registered endpoint.
      """
      def endpoint do
        @endpoint
      end
    end
  end

  @doc false
  defmacro define_supervisor(module) do
    dynamo = Module.get_attribute(module, :config)[:dynamo]

    quote location: :keep do
      @supervisor unquote(dynamo[:supervisor])

      def supervisor do
        @supervisor
      end
    end
  end

  @doc false
  defmacro define_templates_paths(module) do
    dynamo = Module.get_attribute(module, :config)[:dynamo]
    templates_paths = dynamo[:templates_paths]

    { runtime, compiled } =
      if dynamo[:compile_on_demand] do
        { templates_paths, [] }
      else
        Enum.partition(templates_paths, Dynamo.Templates.Finder.precompiled?(&1))
      end

    if compiled != [] do
      module = dynamo[:compiled_templates]
      templates_paths = [module|runtime]
    end

    quote location: :keep do
      @templates_paths unquote(Macro.escape(templates_paths))

      @doc """
      Returns templates paths after being processed.

      If compilation on demand is disabled, templates paths
      that can be precompiled will be precompiled and stored
      into a given module for performance.
      """
      def templates_paths, do: @templates_paths
    end
  end
end