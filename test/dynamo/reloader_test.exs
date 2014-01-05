defmodule Dynamo.LoaderTest do
  use ExUnit.Case

  defp fixture_path do
    Path.expand("../fixtures/reloader", __DIR__)
  end

  setup_all do
    Dynamo.Loader.append_paths([fixture_path])
    :ok
  end

  teardown_all do
    Dynamo.Loader.stop
    :ok
  end

  setup do
    Dynamo.Loader.enable
    :ok
  end

  test "automatically loads code" do
    refute :code.is_loaded(Foo)

    # Compilation on demand works
    assert Foo.foo == 1
    assert Foo.Bar.bar == 2

    # Check clean slate before purging
    assert Process.get(:purge_callback) == nil

    # Prepare a callback
    Dynamo.Loader.on_purge(fn -> Process.put(:purge_callback, :ok) end)

    # Update file
    File.touch!("#{fixture_path}/foo.ex")

    # Purge it!
    assert Dynamo.Loader.conditional_purge == :purged

    # Check state changed
    assert Process.get(:purge_callback) == :ok

    # It was purged!
    refute :code.is_loaded(Foo)
    assert Foo.foo == 1
  end

  test "does not search for erlang modules" do
    assert Dynamo.Loader.load_missing(:cowboy) == :notfound
  end
end
