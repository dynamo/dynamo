Mix.start
Mix.env(:dev)
Mix.shell(Mix.Shell.Process)
System.put_env("MIX_ENV", "dev")

ExUnit.start

:application.ensure_all_started(:hackney)

defmodule MixHelpers do
  import ExUnit.Assertions

  def tmp_path do
    Path.expand("../tmp", __DIR__)
  end

  def tmp_path(extension) do
    Path.join tmp_path, extension
  end

  def in_tmp(which, function) do
    path = tmp_path(which)
    File.rm_rf! path
    File.mkdir_p! path
    File.cd! path, function
  end

  def assert_file(file) do
    assert File.regular?(file), "Expected #{file} to exist, but does not"
  end

  def assert_file(file, match) do
    cond do
      Regex.regex?(match) ->
        &assert_file(file, &1 =~ match)
      is_function(match, 1) ->
        assert_file(file)
        match.(File.read!(file))
    end
  end
end
