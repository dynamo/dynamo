Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.DynamoTest do
  use ExUnit.Case

  import MixHelpers

  test "prints version" do
    Mix.Tasks.Dynamo.run ["-v"]
    assert_received { :mix_shell, :info, ["Dynamo v" <> _] }
  end

  test "throws an error if the designated app name is a module used by dynamo" do
    in_tmp "wrong_name", fn ->
      assert_raise Mix.Error, "the name is already used by a dynamo module", fn ->
        Mix.Tasks.Dynamo.run ["dynamo"]
      end
    end
  end

  test "generates a new dynamo app" do
    in_tmp "my_app", fn ->
      Mix.Tasks.Dynamo.run ["."]

      assert_file "mix.exs", fn(file) ->
        assert file =~ %r(app: :my_app)
        assert file =~ %r(version: "0.0.1")
        assert file =~ %r({ :dynamo)
        assert file =~ %r(github: "elixir-lang/dynamo")
      end

      assert_file "README.md", %r(# MyApp)
      assert_file ".gitignore"

      assert_file "lib/my_app.ex", fn(file) ->
        assert file =~ %r(use Application.Behaviour)
      end

      assert_file "lib/my_app/dynamo.ex", fn(file) ->
        assert file =~ %r(endpoint: ApplicationRouter)
        assert file =~ %r(env: Mix.env)
      end

      assert_file "web/routers/application_router.ex"
      assert_file "web/templates/index.html.eex"

      assert_file "lib/my_app/environments/dev.exs"
      assert_file "lib/my_app/environments/test.exs"
      assert_file "lib/my_app/environments/prod.exs"

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating lib/my_app.ex"] }

      assert_file "test/test_helper.exs"
      assert_file "test/features/home_test.exs"
      assert_file "test/routers/application_router_test.exs"
    end
  end

  test "generates a dynamo app for development" do
    in_tmp "my_dev_app", fn ->
      Mix.Tasks.Dynamo.run [".", "--dev"]

      assert_file "mix.exs", fn(file) ->
        assert file =~ %r(path:)
      end

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating lib/my_dev_app.ex"] }
    end
  end
end
