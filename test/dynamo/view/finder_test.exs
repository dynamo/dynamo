Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.View.PathFinderTest do
  use ExUnit.Case, async: true

  @fixture_path File.expand_path("../../../fixtures/views", __FILE__)
  @path_finder Dynamo.View.PathFinder.new(@fixture_path)

  test "finds available template" do
    path = File.join(@fixture_path, "hello.html.eex")

    assert Dynamo.View.Template[identifier: ^path, key: "hello.html",
      handler: Dynamo.View.EEXHandler, format: "html"] = @path_finder.find "hello.html"
  end

  test "returns all templates" do
    all = @path_finder.all
    assert Enum.find all, fn(t) -> t.key == "hello.html"  end
    assert Enum.find all, fn(t) -> t.key == "module.html" end
  end

  test "returns nil if no template is found" do
    assert @path_finder.find("unknown.html") == nil
  end
end