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
        assert file =~ %r(public_root: :my_app)
      end

      assert_file "config/environments/dev.ex"
      assert_file "config/environments/test.ex"
      assert_file "config/environments/prod.ex"

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating config/app.ex"] }
    end
  end

  test "generates a dynamo app for development" do
    in_tmp "my_app", fn ->
      Mix.Tasks.Dynamo.run [".", "--dev"]

      assert_file "mix.exs", fn(file) ->
        assert file =~ %r(raw:)
      end

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating config/app.ex"] }
    end
  end
end