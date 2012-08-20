Code.require_file "../../test_helper", __FILE__

defmodule Dynamo.ReloaderTest do
  use ExUnit.Case

  defp fixture_path do
    File.expand_path("../../fixtures", __FILE__)
  end

  def setup_all do
    Dynamo.Reloader.start_link([fixture_path])
    Dynamo.Reloader.enable
  end

  def teardown_all do
    Dynamo.Reloader.stop
  end

  test "automatically loads code" do
    refute :code.is_loaded(Foo)

    assert Foo.foo == 1
    assert Foo.Bar.bar == 2

    File.touch!("#{fixture_path}/foo.ex")

    Dynamo.Reloader.conditional_purge

    refute :code.is_loaded(Foo)
    assert Foo.foo == 1
  end
end
