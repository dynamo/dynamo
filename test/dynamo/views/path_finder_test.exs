Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Views.PathFinderTest do
  use ExUnit.Case, async: true

  @view_path File.expand_path("../../../fixtures/views", __FILE__)
  @finder Dynamo.Views.PathFinder.new(@view_path)

  test "finds available template" do
    path = File.join(@view_path, "hello.html.eex")

    assert Dynamo.Views.Template[identifier: ^path,
      handler: "eex", format: "html"] = @finder.find "hello.html"
  end

  test "returns nil if no template is found" do
    assert @finder.find("unknown.html") == nil
  end
end