defmodule Dynamo.Templates.FinderTest do
  use ExUnit.Case, async: true

  @fixture_path Path.expand("../../fixtures/templates", __DIR__)

  test "finds available template" do
    path = Path.join(@fixture_path, "hello.html.eex")

    assert %Dynamo.Template{identifier: ^path, key: "hello.html",
      handler: Dynamo.Templates.EEXHandler, format: "html",
      finder: @fixture_path} = Dynamo.Templates.Finder.find @fixture_path, "hello.html"
  end

  test "returns all templates" do
    all = Dynamo.Templates.Finder.all @fixture_path
    assert Enum.find all, fn(t) -> t.key == "hello.html"  end
    assert Enum.find all, fn(t) -> t.key == "module.html" end
  end

  test "returns nil if no template is found" do
    assert Dynamo.Templates.Finder.find(@fixture_path, "unknown.html") == nil
  end
end
