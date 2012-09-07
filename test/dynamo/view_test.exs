Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.ViewTest do
  use ExUnit.Case, async: true

  @view_paths [Dynamo.View.PathFinder.new(File.expand_path("../../fixtures/views", __FILE__))]

  def setup(_) do
    Dynamo.View.Renderer.start_link
  end

  def teardown(_) do
    Dynamo.View.Renderer.stop
  end

  test "renders a template" do
    body = render "hello.html"
    assert body == "HELLO!"
  end

  test "uses cached template unless it changes" do
    module = render "module.html"
    assert "Elixir-" <> _ = module

    cached = render "module.html"
    assert module == cached

    template = File.expand_path("../../fixtures/views/module.html.eex", __FILE__)

    try do
      File.touch!(template, { { 2030, 1, 1 }, { 0, 0, 0 } })
      not_cached = render "module.html"
      assert module != not_cached
    after
      File.touch!(template, :erlang.universaltime)
    end
  end

  defp render(query) do
    Dynamo.View.render Dynamo.View.find(query, @view_paths), []
  end
end