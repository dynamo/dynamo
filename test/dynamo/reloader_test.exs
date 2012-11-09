Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.ReloaderTest do
  use ExUnit.Case

  defp fixture_path do
    File.expand_path("../../fixtures/reloader", __FILE__)
  end

  def setup_all do
    Dynamo.Reloader.append_paths([fixture_path])
    Dynamo.Reloader.enable
  end

  def teardown_all do
    Dynamo.Reloader.stop
  end

  test "automatically loads code" do
    refute :code.is_loaded(Foo)

    # Compilation on demand works
    assert Foo.foo == 1
    assert Foo.Bar.bar == 2

    # Check clean slate before purging
    assert Process.get(:purge_callback) == nil

    # Prepare a callback
    Dynamo.Reloader.on_purge(fn -> Process.put(:purge_callback, :ok) end)

    # Update file
    File.touch!("#{fixture_path}/foo.ex")

    # Purge it!
    assert Dynamo.Reloader.conditional_purge == :purged

    # Check state changed
    assert Process.get(:purge_callback) == :ok

    # It was purged!
    refute :code.is_loaded(Foo)
    assert Foo.foo == 1
  end

  test "does not search for erlang modules" do
    assert Dynamo.Reloader.load_missing(:cowboy) == :notfound
  end
end
