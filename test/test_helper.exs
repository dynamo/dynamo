Mix.start
Mix.env(:dev)
Mix.shell(Mix.Shell.Process)
System.put_env("MIX_ENV", "dev")

ExUnit.start

:ok = :application.start(:asn1)
:ok = :application.start(:public_key)
:ok = :application.start(:ssl)
:ok = :application.start(:hackney)

defmodule MixHelpers do
  import ExUnit.Assertions

  def tmp_path do
    Path.expand("../../tmp", __FILE__)
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

  def assert_file(file, match) when is_regex(match) do
    &assert_file(file, &1 =~ match)
  end

  def assert_file(file, callback) when is_function(callback, 1) do
    assert_file(file)
    callback.(File.read!(file))
  end
end
