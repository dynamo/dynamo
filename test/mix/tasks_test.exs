defmodule Mix.TasksTest do
  use ExUnit.Case
  import MixHelpers

  test "compiles an application" do
    in_tmp "my_compiled_app", fn ->
      app_with_dynamo_deps_path(:prod)

      output = System.cmd "MIX_ENV=prod mix compile"
      assert output =~ ~r(Compiled web/routers/application_router.ex)
      assert output =~ ~r(Compiled lib/my_app.ex)
      assert output =~ ~r(Compiled lib/my_app/dynamo.ex)
      assert output =~ ~r(Generated my_compiled_app.app)
      assert output =~ ~r(Generated MyApp.Dynamo.CompiledTemplates)
      assert File.regular?("_build/prod/lib/my_compiled_app/ebin/Elixir.MyApp.Dynamo.CompiledTemplates.beam")

      # Can recompile after changes
      File.touch!("web/routers/application_router.ex", { { 2030, 1, 1 }, { 0, 0, 0 } })
      output = System.cmd "MIX_ENV=prod mix compile"
      assert output =~ ~r(Compiled web/routers/application_router.ex)
    end
  end

  test "prints application filters" do
    in_tmp "my_filters_app", fn ->
      app_with_dynamo_deps_path(:dev)

      output = System.cmd "mix dynamo.filters"
      assert output =~ ~r(filter Dynamo.Filters.Head)
      assert output =~ ~r(filter \{Dynamo.Filters.Loader, *true, *true\})
      assert output =~ ~r(ApplicationRouter.service/1)
    end
  end

  test "runs application code" do
    in_tmp "my_run_app", fn ->
      app_with_dynamo_deps_path(:dev)

      output = System.cmd ~s{mix run -e "IO.inspect HelloRouter.__info__(:module)"}
      assert output =~ ~r(HelloRouter)
    end
  end

  test "tests application code" do
    in_tmp "my_test_app", fn ->
      app_with_dynamo_deps_path(:test)

      File.write! "test/routers/hello_router_test.exs", """
      defmodule HelloRouterTest do
        use MyApp.TestCase

        test "hello router is autoloaded" do
          assert HelloRouter.__info__(:module)
        end
      end
      """

      output = System.cmd ~s{unset MIX_ENV; mix test}
      assert output =~ ~r(3 tests, 0 failures)
    end
  end

  defp app_with_dynamo_deps_path(env) do
    Mix.Tasks.Dynamo.run [".", "--module", "MyApp", "--dev"]
    File.cp! "../../mix.lock", "mix.lock"

    File.mkdir_p!("_build/#{env}")
    File.cp_r!("../../_build/test", "_build/#{env}")

    :ok = :file.make_symlink("../../deps", "deps")

    File.write! "mix.exs",
      Regex.replace(~r"deps: deps", File.read!("mix.exs"), ~s(deps: deps, deps_path: "../../deps"))

    File.write! "web/routers/hello_router.ex", """
    defmodule HelloRouter do
      use Dynamo.Router
    end
    """
  end
end
