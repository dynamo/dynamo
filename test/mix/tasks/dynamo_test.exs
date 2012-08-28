Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.DynamoTest do
  use ExUnit.Case

  import MixHelpers

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
      Mix.Tasks.Dynamo.run [".", "--dev"]

      File.write! "mix.exs", Regex.replace(%r"deps: deps", File.read!("mix.exs"), %b(deps: deps, deps_path: "../../deps"))
      output = System.cmd "MIXENV=prod mix compile"

      assert output =~ %r(Compiled app/routers/application_router.ex)
      assert output =~ %r(Compiled config/app.ex)
      assert output =~ %r(Generated my_compiled_app.app)
    end
  end
end