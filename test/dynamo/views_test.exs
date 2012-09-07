Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.ViewsTest do
  use ExUnit.Case, async: true

  @view_paths [Dynamo.Views.PathFinder.new(File.expand_path("../../fixtures/views", __FILE__))]

  def setup(_) do
    Dynamo.Views.Renderer.start_link
  end

  def teardown(_) do
    Dynamo.Views.Renderer.stop
  end

  test "renders a template" do
    body = Dynamo.Views.render "hello.html", @view_paths, []
    assert body == "HELLO!"
  end
end