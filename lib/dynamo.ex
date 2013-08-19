defmodule Dynamo do
  @moduledoc """
  Run, Dynamo, Run!

  This is the main module in the Dynamo repository. It allows
  users to configure the Dynamo framework and define their
  own Dynamos.

  A very simple Dynamo can be defined as follow:

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
        otp_app: :myapp,
        static_root:  "priv/static",
        static_route: "/static"

  The available `:dynamo` configurations are:

  * `:compile_on_demand` - Compiles modules as they are needed
  * `:env` - The environment this Dynamo runs on
  * `:endpoint` - The endpoint to dispatch requests too
  * `:exceptions_editor` - Some exception handlers show editors information
     to help debugging (defaults to the DYNAMO_EDITOR environment variable)
  * `:exceptions_handler` - How to handle and display exceptions (defaults to `Exceptions.Public`)
  * `:reload_modules` - Reload modules after they are changed
  * `:session_store` - The session store to be used, may be `CookieStore` and `ETSStore`
  * `:session_options` - The session options to be used
  * `:source_paths` - The paths to search when compiling modules on demand
  * `:static_route` - The route to serve static assets
  * `:static_root` - The location static assets are defined. It is a path from the otp_app root
  * `:supervisor` - The supervisor local node name
  * `:templates_paths` - The paths to find templates

  Check `Dynamo.Base` for more information on `config` and
  other initialize configuration.

  ## Filters

  A Dynamo also contains a set of filters that are meant
  to be used on all requests. Some of these filters are added
  based on the configuration options above. Others are included by
  default under the following conditions:

  * `Dynamo.Filters.Static` - when a static_route and static_root are set,
     this filter is added to serve static assets;
  * `Dynamo.Filters.Head` - converts HEAD requests to GET, added by default;
  * `Dynamo.Filters.Loader` - when `:compile_on_demand` or `:reload_modules`
    configs are set to true, this filter is added to compiled and reloaded
    code on demand;
  * `Dynamo.Filters.Session` - when a `:session_store` is configured, it adds
    session functionality to the Dynamo;
  * `Dynamo.Filters.Exceptions` - responsible for logging and handling
    exceptions, added by default;

  Filters can be added and removed using `filter` and `remove_filter`
  macros. You can get the list of all dynamos filters using:
  `mix dynamo.filters`.

  For more information, check `Dynamo.Router.Filters` docs.

  ## Initialization

  A Dynamo allows you to register initializers which are
  invoked when the dynamo starts. A Dynamo is initialized
  in three steps:

  * The `:dynamos` registered in your project are compiled
  * The dynamos supervision trees are started via `DYNAMO.start_link`
  * A dynamo is hooked into a web server via `DYNAMO.run`

  The step 2 can be extended via initializers. For example:

      defmodule MyDynamo do
        use Dynamo

        initializer :some_config do
          # Connect to the database
        end
      end

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
    Dynamo.App.start

    setup =
      quote do
        if Module.get_attribute(__MODULE__, :dynamo_router) do
          raise "Dynamo needs to be used before Dynamo.Router"
        end

        @before_compile { unquote(__MODULE__), :load_env_file }
        @before_compile { unquote(__MODULE__), :define_endpoint }
        @before_compile { unquote(__MODULE__), :define_filters }
        @before_compile { unquote(__MODULE__), :define_templates_paths }
        @before_compile { unquote(__MODULE__), :define_static }
        @before_compile { unquote(__MODULE__), :define_root }

        use Dynamo.Utils.Once
        alias Dynamo.Filters.Session,    as: Session
        alias Dynamo.Filters.Exceptions, as: Exceptions

        use_once Dynamo.Base
        use_once Dynamo.Router.Filters

        config :dynamo, unquote(default_dynamo_config(__CALLER__))
        config :server, [handler: Dynamo.Cowboy, port: 4000]
      end

    definitions =
      quote location: :keep do
        @doc """
        Starts the Dynamo supervisor and run all
        registered initializers.
        """
        def start_link(opts // []) do
          info = Dynamo.Supervisor.start_link(config[:dynamo][:supervisor], opts)
          run_initializers
          info
        end

        @doc """
        Runs the Dynamo in the configured web server.
        """
        def run(options // []) do
          dynamo  = config[:dynamo]
          options = Keyword.put(options, :ssl, config[:ssl])
          options = Keyword.put(options, :env, dynamo[:env])
          options = Keyword.put(options, :otp_app, dynamo[:otp_app])
          options = Keyword.merge(config[:server], options)
          options[:handler].run(__MODULE__, options)
        end

        initializer :start_dynamo_reloader do
          dynamo = config[:dynamo]

          if dynamo[:compile_on_demand] do
            callback = fn
              path, acc when is_binary(path) ->
                (path |> Path.expand(root) |> Path.wildcard) ++ acc
              _, acc ->
                acc
            end

            source    = Enum.reduce dynamo[:source_paths], [], callback
            templates = Enum.reduce dynamo[:templates_paths], [], callback

            Dynamo.Loader.append_paths(source -- templates)
            Dynamo.Loader.enable

            if Code.ensure_loaded?(IEx) and IEx.started? do
              IEx.after_spawn(fn -> Dynamo.Loader.enable end)
            end
          end
        end

        initializer :start_dynamo_renderer do
          precompiled = Enum.all?(templates_paths, Dynamo.Templates.Finder.requires_precompilation?(&1))
          unless precompiled do
            supervisor = config[:dynamo][:supervisor]
            renderer   = templates_server()
            Dynamo.Supervisor.start_child(supervisor, Dynamo.Templates.Renderer, [renderer])

            if config[:dynamo][:compile_on_demand] do
              Dynamo.Loader.on_purge(fn -> Dynamo.Templates.Renderer.clear(renderer) end)
            end
          end
        end
      end

    quote do
      unquote(setup)
      unquote(definitions)
    end
  end

  ## Helpers

  defp default_dynamo_config(env) do
    [ cache_static: true,
      compile_on_demand: true,
      compiled_templates: env.module.CompiledTemplates,
      env: "prod",
      environments_path: Path.expand("../environments", env.file),
      exceptions_editor: System.get_env("DYNAMO_EDITOR"),
      exceptions_handler: Dynamo.Filters.Exceptions.Public,
      reload_modules: false,
      session_options: [],
      source_paths: ["web/*"],
      static_root: "priv/static",
      supervisor: env.module.Supervisor,
      templates_paths: ["web/templates"] ]
  end

  ## __before_compile__ callbacks

  @doc false
  defmacro load_env_file(_) do
    dynamo = Module.get_attribute(__CALLER__.module, :config)[:dynamo]
    dir = dynamo[:environments_path]
    env = dynamo[:env]
    if dir && File.dir?(dir) do
      file = "#{dir}/#{env}.exs"
      Code.string_to_quoted! File.read!(file), file: file
    end
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

    if dynamo[:static_route] do
      static  = Dynamo.Filters.Static.new(dynamo[:static_route], dynamo[:static_root])
      filters = [static|filters]
    end

    if dynamo[:compile_on_demand] || dynamo[:reload_modules] do
      reloader = Dynamo.Filters.Loader.new(dynamo[:compile_on_demand], dynamo[:reload_modules])
      filters  = [reloader|filters]
    end

    if dynamo[:reload_modules] && !dynamo[:compile_on_demand] do
      raise "Cannot have reload_modules set to true and compile_on_demand set to false"
    end

    if dynamo[:session_store] && dynamo[:session_options] do
      session = Dynamo.Filters.Session.new(dynamo[:session_store], dynamo[:session_options])
      filters = [session|filters]
    end

    if dynamo[:exceptions_handler] do
      exceptions = Dynamo.Filters.Exceptions.new(dynamo[:exceptions_handler])
      filters    = [exceptions|filters]
    end

    filters = [Dynamo.Filters.Head|filters]
    filters
  end

  @doc false
  defmacro define_endpoint(env) do
    endpoint = Module.get_attribute(env.module, :config)[:dynamo][:endpoint] |> Macro.escape

    if endpoint do
      quote location: :keep do
        @doc """
        Receives a connection and dispatches it to #{inspect unquote(endpoint)}
        """
        def service(conn) do
          unquote(endpoint).service(conn)
        end
      end
    end
  end

  @doc false
  defmacro define_templates_paths(env) do
    module = env.module
    dynamo = Module.get_attribute(module, :config)[:dynamo]
    templates_paths = dynamo[:templates_paths]

    { runtime, to_compile } =
      if dynamo[:compile_on_demand] do
        { templates_paths, [] }
      else
        Enum.partition(templates_paths, Dynamo.Templates.Finder.requires_precompilation?(&1))
      end

    if to_compile != [] do
      module = dynamo[:compiled_templates]
      templates_paths = [module|runtime]
    end

    templates_server = dynamo[:supervisor].TemplatesServer

    templates_paths  = lc path inlist templates_paths do
      if is_binary(path) do
        quote do: Path.expand(unquote(path), root)
      else
        Macro.escape(path)
      end
    end

    quote location: :keep do
      @doc """
      Returns templates paths after being processed.

      If compilation on demand is disabled, templates paths
      that can be precompiled will be precompiled and stored
      into a given module for performance.
      """
      def templates_paths, do: unquote(templates_paths)

      @doc """
      The worker responsible for rendering templates.
      """
      def templates_server, do: unquote(templates_server)
    end
  end

  @doc false
  defmacro define_static(env) do
    module = env.module
    dynamo = Module.get_attribute(module, :config)[:dynamo]
    supervisor = dynamo[:supervisor]

    if dynamo[:static_route] do
      quote location: :keep do
        @doc """
        Returns the static ets table and server name
        used by this Dynamo.
        """
        def static_cache do
          { unquote(supervisor.StaticTable), unquote(supervisor.StaticServer) }
        end

        initializer :start_dynamo_static do
          Dynamo.Supervisor.start_child(config[:dynamo][:supervisor], Dynamo.Static, [__MODULE__])
        end
      end
    end
  end

  @doc false
  defmacro define_root(env) do
    module = env.module
    dynamo = Module.get_attribute(module, :config)[:dynamo]
    root   =
      cond do
        dynamo[:root] -> dynamo[:root]
        nil?(dynamo[:otp_app]) -> File.cwd!
        true -> nil
      end

    if root do
      quote location: :keep do
        @doc """
        Returns the root path for this Dynamo.
        """
        def root, do: unquote(root)
      end
    else
      app = dynamo[:otp_app]
      tmp = "#{app}/tmp/#{dynamo[:env]}/#{app}"

      quote location: :keep do
        @doc """
        Returns the root path for this Dynamo
        based on the OTP app directory.
        """
        def root do
          case :code.lib_dir(unquote(app)) do
            list when is_list(list) ->
              bin  = String.from_char_list!(list)
              size = size(bin)

              if size > unquote(size(tmp)) do
                :binary.replace bin, unquote(tmp),
                  unquote(atom_to_binary(app)), scope: { size, unquote(-size(tmp)) }
              else
                bin
              end
            _ ->
              raise "could not find OTP app #{unquote(dynamo[:otp_app])} for #{inspect __MODULE__}. " <>
                "This may happen if the directory name is different than the application name."
          end
        end
      end
    end
  end
end
