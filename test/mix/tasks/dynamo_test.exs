Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.DynamoTest do
  use ExUnit.Case

  import MixHelpers

  test "prints version" do
    Mix.Tasks.Dynamo.run ["-v"]
    assert_received { :mix_shell, :info, ["Dynamo v" <> _] }
  end

  test "generates a new dynamo app" do
    in_tmp "my_app", fn ->
      Mix.Tasks.Dynamo.run ["."]

      assert_file "mix.exs", fn(file) ->
        assert file =~ %r(app: :my_app)
        assert file =~ %r(version: "0.0.1")
        assert file =~ %r({ :dynamo)
        assert file =~ %r(git: "https://github.com/josevalim/dynamo.git")
      end

      assert_file "README.md", %r(# MyApp)
      assert_file ".gitignore"

      assert_file "config/app.ex", fn(file) ->
        assert file =~ %r(endpoint ApplicationRouter)
        assert file =~ %r(otp_app: :my_app)
      end

      assert_file "config/environments/dev.exs"
      assert_file "config/environments/test.exs"
      assert_file "config/environments/prod.exs"

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating config/app.ex"] }
    end
  end

  test "generates a dynamo app for development" do
    in_tmp "my_dev_app", fn ->
      Mix.Tasks.Dynamo.run [".", "--dev"]

      assert_file "mix.exs", fn(file) ->
        assert file =~ %r(raw:)
      end

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating config/app.ex"] }
    end
  end

  test "generates and compiles an application" do
    in_tmp "my_compiled_app", fn ->
      app_with_dynamo_deps_path

      ## Cannot boot production without compiling
      output = System.cmd "MIXENV=prod mix server"
      assert output =~ %r(could not find endpoint ApplicationRouter, please ensure it was compiled)

      output = System.cmd "MIXENV=prod mix compile"
      assert output =~ %r(Compiled app/routers/application_router.ex)
      assert output =~ %r(Compiled config/app.ex)
      assert output =~ %r(Generated my_compiled_app.app)

      ## Cannot boot development after compiling
      output = System.cmd "MIXENV=dev mix server"
      assert output =~ %r(the dynamo application MyCompiledApp is already loaded)
    end
  end

  defp app_with_dynamo_deps_path do
    Mix.Tasks.Dynamo.run [".", "--dev"]
    File.write! "mix.exs", Regex.replace(%r"deps: deps", File.read!("mix.exs"), %b(deps: deps, deps_path: "../../deps"))
  end
end