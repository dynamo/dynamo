defmodule Mix.Tasks.Dynamo do
  use Mix.Task

  import Mix.Generator
  import Mix.Utils, only: [camelize: 1, underscore: 1]

  @version Dynamo.Mixfile.project[:version]
  @shortdoc "Create a new Dynamo project"

  @moduledoc """
  Creates a new Dynamo project.
  It expects the path of the project as argument.

      mix dynamo [-v] PATH [--app APP] [--module MODULE]

  A project at the given PATH  will be created. The
  application name and module name will be retrieved
  from the path, unless `-app` or `--module` is given.

  ## Examples

      mix dynamo hello_world

  Is equivalent to:

      mix dynamo hello_world --app hello_world --module HelloWorld

  Use -v to print mix version:

      mix dynamo -v

  """
  def run(["-v"]) do
    Mix.shell.info "Dynamo v#{@version}"
  end

  def run(argv) do
    { opts, argv } = OptionParser.parse(argv, flags: [:dev])
    case argv do
      [] ->
        raise Mix.Error, message: "expected PATH to be given, please use `mix dynamo PATH`"
      [path|_] ->
        name = opts[:app] || File.basename(File.expand_path(path))
        check_project_name!(name)
        File.mkdir_p!(path)
        File.cd!(path, fn -> do_generate(underscore(name), opts) end)
    end
  end

  defp do_generate(app, opts) do
    mod = opts[:module] || camelize(app)
    lib = underscore(mod)

    dynamo = if opts[:dev] do
      %b(raw: "#{File.expand_path("../../../..", __FILE__)}")
    else
      %b(github: "josevalim/dynamo")
    end

    assigns = [app: app, mod: mod, dynamo: dynamo, version: @version]

    create_file "README.md",  readme_template(assigns)
    create_file ".gitignore", gitignore_text
    create_file "mix.lock",   mixlock_text
    create_file "mix.exs",    mixfile_template(assigns)

    create_directory "app"
    create_directory "app/routers"
    create_file "app/routers/application_router.ex", app_router_template(assigns)

    create_directory "app/views"
    create_file "app/views/index.html.eex", app_view_template(assigns)

    create_directory "lib"
    create_file "lib/#{lib}.ex", lib_app_template(assigns)

    create_directory "lib/#{lib}/environments"
    create_file "lib/#{lib}/environments/dev.exs",  lib_dev_template(assigns)
    create_file "lib/#{lib}/environments/test.exs", lib_test_template(assigns)
    create_file "lib/#{lib}/environments/prod.exs", lib_prod_template(assigns)

    create_directory "priv"
    create_directory "priv/static"

    create_directory "test"
    create_file "test/test_helper.exs", test_helper_template(assigns)

    create_directory "test/features"
    create_file "test/features/home_test.exs", test_features_text

    create_directory "test/routers"
    create_file "test/routers/application_router_test.exs", test_routers_text
  end

  defp check_project_name!(name) do
    unless name =~ %r/^[a-z][\w_]+$/i do
      raise Mix.Error, message: "project path must start with a letter and have only letters, numbers and underscore"
    end
  end

   embed_template :readme, """
   # <%= @mod %>

   ** TODO: Add description **
   """

   embed_text :gitignore, """
   /ebin
   /deps
   erl_crash.dump
   """

  embed_template :mixfile, %B"""
  defmodule <%= @mod %>.Mixfile do
    use Mix.Project

    def project do
      [ app: :<%= @app %>,
        version: "0.0.1",
        compile_path: "ebin/#{Mix.env}",
        prepare_task: "dynamo.start",
        dynamos: [<%= @mod %>],
        compilers: [:elixir, :dynamo, :app],
        deps: deps ]
    end

    # Configuration for the OTP application
    def application do
      []
    end

    defp deps do
      [ { :ranch, %r(.*), github: "extend/ranch" },
        { :cowboy, %r(.*), github: "extend/cowboy" },
        { :dynamo, "<%= @version %>", <%= @dynamo %> } ]
    end
  end
  """

  embed_text :mixlock, from_file("../../../../mix.lock")

  embed_template :app_router, """
  defmodule ApplicationRouter do
    use Dynamo.Router

    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    fetch [:cookies, :params]

    # It is common to break your application in many
    # routers forwarding the requests between them
    # forward "/posts", to: PostsRouter

    get "/" do
      conn = conn.assign(:title, "Welcome to Dynamo!")
      render conn, "index.html"
    end
  end
  """

  embed_template :app_view, """
  <!DOCTYPE HTML>
  <html>
  <head>
    <title><%= @title %></title>
  </head>
  <body>
    <h3>Welcome to Dynamo!</h3>
    <ol>
      <li>Change this view at <code>app/views/index.html.eex</code></li>
      <li>Add new routes at <code>app/routers/application_router.ex</code></li>
      <li>Deploy to production with <code>MIX_ENV=prod mix do compile, server</li>
    </ol>
  </body>
  </html>
  """

  embed_template :lib_app, """
  Dynamo.start(Mix.env)

  defmodule <%= @mod %> do
    use Dynamo.App

    endpoint ApplicationRouter

    config :dynamo,
      # The OTP application associated to this app
      # This is the name of the .app file generated by mix
      otp_app: :<%= @app %>,
      # The route from where public assets are served
      # You can turn off static assets by setting it to false
      static_route: "/static"

    # Default functionality available in views
    views do
      use Dynamo.Helpers
    end

    initializer :start_otp_app do
      :application.start config[:dynamo][:otp_app]
    end
  end
  """

  embed_template :lib_dev, """
  config :dynamo,
    # Compile modules as they are accessed.
    # This makes development easy as we don't
    # need to explicitly compile files.
    compile_on_demand: true,

    # Every time a module in app changes, we
    # will clean up defined modules and pick
    # up the latest versions.
    reload_modules: true,

    # Run on port 4000 for development
    port: 4000
  """

  embed_template :lib_test, """
  config :dynamo,
    # For testing we compile modules on demand,
    # but there isn't a need to reload them.
    compile_on_demand: true,
    reload_modules: false,
    port: 8888
  """

  embed_template :lib_prod, """
  config :dynamo,
    # On production, modules are compiled up-front.
    compile_on_demand: false,
    reload_modules: false,
    port: 80
  """

  embed_text :test_features, """
  Code.require_file "../../test_helper.exs", __FILE__

  # Feature tests goes through the Dynamo.under_test
  # and are meant to test the full stack.
  defmodule HomeTest do
    use ExUnit.Case
    use Dynamo.HTTP.Case

    test "returns OK" do
      conn = get("/")
      assert conn.status == 200
    end
  end
  """

  embed_text :test_routers, """
  Code.require_file "../../test_helper.exs", __FILE__

  defmodule ApplicationRouterTest do
    use ExUnit.Case
    use Dynamo.HTTP.Case

    # Sometimes it may be convenient to test a specific
    # aspect of a router in isolation. For such, we just
    # need to set the @app to the router under test.
    @app ApplicationRouter

    test "returns OK" do
      conn = get("/")
      assert conn.status == 200
    end
  end
  """

  embed_template :test_helper, """
  Dynamo.under_test(<%= @mod %>)
  ExUnit.start

  # Enable reloading in each ExUnit process
  ExUnit.after_spawn fn ->
    Dynamo.Reloader.enable
  end
  """
end