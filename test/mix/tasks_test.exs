Code.require_file "../../test_helper.exs", __FILE__

defmodule Mix.TasksTest do
  use ExUnit.Case
  import MixHelpers

  test "compiles an application" do
    in_tmp "my_compiled_app", fn ->
      app_with_dynamo_deps_path

      output = System.cmd "MIX_ENV=prod mix compile"
      assert output =~ %r(Compiled web/routers/application_router.ex)
      assert output =~ %r(Compiled lib/my_app.ex)
      assert output =~ %r(Compiled lib/my_app/dynamo.ex)
      assert output =~ %r(Generated my_compiled_app.app)
      assert output =~ %r(Generated MyApp.Dynamo.CompiledTemplates)
      assert File.regular?("ebin/Elixir-MyApp-Dynamo-CompiledTemplates.beam")

      # Can recompile after changes
      File.touch!("web/routers/application_router.ex", { { 2030, 1, 1 }, { 0, 0, 0 } })
      output = System.cmd "MIX_ENV=prod mix compile"
      assert output =~ %r(Compiled web/routers/application_router.ex)

      # TODO: Get rid of this
      File.rm_rf "ebin"

      # Can compile for other environments too
      output = System.cmd "MIX_ENV=dev mix compile"
      assert output =~ %r(Generated my_compiled_app.app)
    end
  end

  test "prints application filters" do
    in_tmp "my_filters_app", fn ->
      app_with_dynamo_deps_path

      output = System.cmd "mix dynamo.filters"
      assert output =~ %r(filter Dynamo.Filters.Head)
      assert output =~ %r(filter \{Dynamo.Filters.Loader,true,true\})
      assert output =~ %r(ApplicationRouter.service/1)

      # Check it works with first compilation in prod
      output = System.cmd "MIX_ENV=prod mix do compile, dynamo.filters"
      refute output =~ %r(Dynamo.Filters.Loader)
      assert output =~ %r(ApplicationRouter.service/1)

      # Check that noop compile also works
      output = System.cmd "MIX_ENV=prod mix do compile, dynamo.filters"
      refute output =~ %r(Dynamo.Filters.Loader)
      assert output =~ %r(ApplicationRouter.service/1)
    end
  end

  test "runs application code" do
    in_tmp "my_run_app", fn ->
      app_with_dynamo_deps_path

      output = System.cmd %b{mix run "IO.inspect HelloRouter.__info__(:module)"}
      assert output =~ %r(HelloRouter)

      # TODO: Get rid of this
      File.rm_rf "ebin"

      output = System.cmd %b{MIX_ENV=prod mix do compile, run "IO.inspect HelloRouter.__info__(:module)"}
      assert output =~ %r(HelloRouter)
    end
  end

  test "tests application code" do
    in_tmp "my_test_app", fn ->
      app_with_dynamo_deps_path

      File.write! "test/routers/hello_router_test.exs", """
      Code.require_file "../../test_helper.exs", __FILE__

      defmodule HelloRouterTest do
        use MyApp.TestCase

        test "hello router is autoloaded" do
          assert HelloRouter.__info__(:module)
        end
      end
      """

      output = System.cmd %b{mix test}
      assert output =~ %r(3 tests, 0 failures)
    end
  end

  test "warns on missing dependencies" do
    in_tmp "missing_deps", fn ->
      Mix.Tasks.Dynamo.run [".", "--dev"]
      error = %r(Some dependencies are out of date)

      output = System.cmd "mix server"
      assert output =~ error

      output = System.cmd "MIX_ENV=prod mix server"
      assert output =~ error
    end
  end

  defp app_with_dynamo_deps_path do
    Mix.Tasks.Dynamo.run [".", "--module", "MyApp", "--dev"]
    File.cp! "../../mix.lock", "mix.lock"

    File.write! "mix.exs",
      Regex.replace(%r"deps: deps", File.read!("mix.exs"), %b(deps: deps, deps_path: "../../deps"))

    File.write! "web/routers/hello_router.ex", """
    defmodule HelloRouter do
      use Dynamo.Router
    end
    """
  end
end
