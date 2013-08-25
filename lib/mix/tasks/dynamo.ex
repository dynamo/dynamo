defmodule Mix.Tasks.Dynamo do
  use Mix.Task

  import Mix.Generator
  import Mix.Utils, only: [camelize: 1, underscore: 1]

  @version Dynamo.Mixfile.project[:version]
  @shortdoc "Create a new Dynamo project"

  @moduledoc """
  Creates a new Dynamo project.
  It expects the path of the project as argument.

      mix dynamo [-v] PATH [--module MODULE]

  A project at the given PATH  will be created. The
  application name and module name will be retrieved
  from the path, unless `--module` is given.

  ## Examples

      mix dynamo hello_world

  Is equivalent to:

      mix dynamo hello_world --module HelloWorld

  Use -v to print mix version:

      mix dynamo -v

  """
  def run(["-v"]) do
    Mix.shell.info "Dynamo v#{@version}"
  end

  def run(argv) do
    parsed = OptionParser.parse(argv, switches: [dev: :boolean])

    case elem(parsed, 1) do
      [] ->
        raise Mix.Error, message: "expected PATH to be given, please use `mix dynamo PATH`"
      [path|_] ->
        name = Path.basename(Path.expand(path))
        check_project_name!(name)
        File.mkdir_p!(path)
        File.cd!(path, fn -> do_generate(name, elem(parsed, 0)) end)
    end
  end

  defp do_generate(app, opts) do
    mod = opts[:module] || camelize(app)
    lib = underscore(mod)

    dynamo = if opts[:dev] do
      %b(path: "#{Path.expand("../../../..", __FILE__)}")
    else
      %b(github: "elixir-lang/dynamo")
    end

    assigns = [app: app, mod: mod, dynamo: dynamo, version: @version]

    create_file "README.md",  readme_template(assigns)
    create_file ".gitignore", gitignore_text
    create_file "mix.lock",   mixlock_text
    create_file "mix.exs",    mixfile_template(assigns)

    create_directory "web"
    create_directory "web/routers"
    create_file "web/routers/application_router.ex", web_router_template(assigns)

    create_directory "web/templates"
    create_file "web/templates/index.html.eex", web_index_template(assigns)

    create_directory "lib"
    create_file "lib/#{lib}.ex", lib_template(assigns)

    create_directory "lib/#{lib}"
    create_file "lib/#{lib}/dynamo.ex", lib_dynamo_template(assigns)

    create_directory "lib/#{lib}/environments"
    create_file "lib/#{lib}/environments/dev.exs",  lib_dev_template(assigns)
    create_file "lib/#{lib}/environments/test.exs", lib_test_template(assigns)
    create_file "lib/#{lib}/environments/prod.exs", lib_prod_template(assigns)

    create_directory "priv"
    create_directory "priv/static"
    create_file "priv/static/favicon.ico", ""

    create_directory "test"
    create_file "test/test_helper.exs", test_helper_template(assigns)

    create_directory "test/features"
    create_file "test/features/home_test.exs", test_features_template(assigns)

    create_directory "test/routers"
    create_file "test/routers/application_router_test.exs", test_routers_template(assigns)
  end

  defp check_project_name!(name) do
    unless name =~ %r/^[a-z][\w_]+$/ do
      raise Mix.Error, message: "project path must start with a letter and have only lowercase letters, numbers and underscore"
    end
    if Code.ensure_loaded?(Module.concat([String.capitalize(name)])) do
      raise Mix.Error, message: "the name is already used by a dynamo module"
    end
  end

  embed_template :readme, """
  # <%= @mod %>

  This is a project built with Elixir that uses Dynamo to serve web requests.

  Resources:

  * [Elixir website](http://elixir-lang.org/)
  * [Elixir getting started guide](http://elixir-lang.org/getting_started/1.html)
  * [Elixir docs](http://elixir-lang.org/docs)
  * [Dynamo source code](https://github.com/elixir-lang/dynamo)
  * [Dynamo guides](https://github.com/elixir-lang/dynamo#learn-more)
  * [Dynamo docs](http://elixir-lang.org/docs/dynamo)
  """

  embed_text :gitignore, """
  /ebin
  /deps
  /tmp/dev
  /tmp/test
  erl_crash.dump
  """

  embed_template :mixfile, %B"""
  defmodule <%= @mod %>.Mixfile do
    use Mix.Project

    def project do
      [ app: :<%= @app %>,
        version: "0.0.1",
        dynamos: [<%= @mod %>.Dynamo],
        compilers: [:elixir, :dynamo, :app],
        env: [prod: [compile_path: "ebin"]],
        compile_path: "tmp/#{Mix.env}/<%= @app %>/ebin",
        deps: deps ]
    end

    # Configuration for the OTP application
    def application do
      [ applications: [:cowboy, :dynamo],
        mod: { <%= @mod %>, [] } ]
    end

    defp deps do
      [ { :cowboy, github: "extend/cowboy" },
        { :dynamo, "<%= @version %>", <%= @dynamo %> } ]
    end
  end
  """

  embed_text :mixlock, from_file("../../../../mix.lock")

  embed_template :web_router, """
  defmodule ApplicationRouter do
    use Dynamo.Router

    prepare do
      # Pick which parts of the request you want to fetch
      # You can comment the line below if you don't need
      # any of them or move them to a forwarded router
      conn.fetch([:cookies, :params])
    end

    # It is common to break your Dynamo into many
    # routers, forwarding the requests between them:
    # forward "/posts", to: PostsRouter

    get "/" do
      conn = conn.assign(:title, "Welcome to Dynamo!")
      render conn, "index.html"
    end
  end
  """

  embed_template :web_index, """
  <!DOCTYPE HTML>
  <html>
  <head>
    <title><%%= @title %></title>
    <link rel="shortcut icon" href="/static/favicon.ico" />
  </head>
  <body>
    <h3>Welcome to Dynamo!</h3>
    <ol>
      <li>Change this template at <code>web/templates/index.html.eex</code></li>
      <li>Add new routes at <code>web/routers/application_router.ex</code></li>
      <li>Deploy to production with <code>MIX_ENV=prod mix do compile, server</code></li>
    </ol>
  </body>
  </html>
  """

  embed_template :lib, """
  defmodule <%= @mod %> do
    use Application.Behaviour

    @doc \"""
    The application callback used to start this
    application and its Dynamos.
    \"""
    def start(_type, _args) do
      <%= @mod %>.Dynamo.start_link([max_restarts: 5, max_seconds: 5])
    end
  end
  """

  embed_template :lib_dynamo, """
  defmodule <%= @mod %>.Dynamo do
    use Dynamo

    config :dynamo,
      # The environment this Dynamo runs on
      env: Mix.env,

      # The OTP application associated with this Dynamo
      otp_app: :<%= @app %>,

      # The endpoint to dispatch requests to
      endpoint: ApplicationRouter,

      # The route from which static assets are served
      # You can turn off static assets by setting it to false
      static_route: "/static"

    # Uncomment the lines below to enable the cookie session store
    # config :dynamo,
    #   session_store: Session.CookieStore,
    #   session_options:
    #     [ key: "_<%= @app %>_session",
    #       secret: "<%= :crypto.strong_rand_bytes(48) |> :base64.encode %>"]

    # Default functionality available in templates
    templates do
      use Dynamo.Helpers
    end
  end
  """

  embed_template :lib_dev, """
  config :dynamo,
    # Compile modules as they are accessed.
    # This makes development easy as we don't
    # need to explicitly compile files.
    compile_on_demand: true,

    # Every time a module in web/ changes, we
    # will clean up defined modules and pick
    # up the latest versions.
    reload_modules: true,

    # Do not cache static assets, so they
    # are reloaded for every page in development
    cache_static: false,

    # Show a nice debugging exception page
    # in development
    exceptions_handler: Exceptions.Debug

  # Run on port 4000 for development
  config :server, port: 4000
  """

  embed_template :lib_test, """
  config :dynamo,
    # For testing we compile modules on demand,
    # but there isn't a need to reload them.
    compile_on_demand: true,
    reload_modules: false

  config :server, port: 8888
  """

  embed_template :lib_prod, """
  config :dynamo,
    # In production, modules are compiled up-front.
    compile_on_demand: false,
    reload_modules: false

  config :server,
    port: 8888,
    acceptors: 100,
    max_connections: 10000

  # config :ssl,
  #  port: 8889,
  #  keyfile: "/var/www/key.pem",
  #  certfile: "/var/www/cert.pem"
  """

  embed_template :test_features, """
  # Feature tests go through the Dynamo.under_test
  # and are meant to test the full stack.
  defmodule HomeTest do
    use <%= @mod %>.TestCase
    use Dynamo.HTTP.Case

    test "returns OK" do
      conn = get("/")
      assert conn.status == 200
    end
  end
  """

  embed_template :test_routers, """
  defmodule ApplicationRouterTest do
    use <%= @mod %>.TestCase
    use Dynamo.HTTP.Case

    # Sometimes it may be convenient to test a specific
    # aspect of a router in isolation. For such, we just
    # need to set the @endpoint to the router under test.
    @endpoint ApplicationRouter

    test "returns OK" do
      conn = get("/")
      assert conn.status == 200
    end
  end
  """

  embed_template :test_helper, """
  Dynamo.under_test(<%= @mod %>.Dynamo)
  Dynamo.Loader.enable
  ExUnit.start

  defmodule <%= @mod %>.TestCase do
    use ExUnit.CaseTemplate

    # Enable code reloading on test cases
    setup do
      Dynamo.Loader.enable
      :ok
    end
  end
  """
end
