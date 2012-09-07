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

      assert_file "app/routers/application_router.ex"
      assert_file "app/views/hello.html.eex"

      assert_file "config/environments/dev.exs"
      assert_file "config/environments/test.exs"
      assert_file "config/environments/prod.exs"

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating config/app.ex"] }

      assert_file "test/test_helper.exs"
      assert_file "test/features/home_test.exs"
      assert_file "test/routers/application_router_test.exs"
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
end