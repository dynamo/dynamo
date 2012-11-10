Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.TemplatesTest do
  use ExUnit.Case

  @fixture_path File.expand_path("../../fixtures/templates", __FILE__)
  @path_finder  Dynamo.Templates.PathFinder.new(@fixture_path)

  test "renders a template" do
    body = render "hello.html"
    assert body == "HELLO!"
  end

  test "uses cached template unless it changes" do
    module = render "module.html"
    assert "Elixir-" <> _ = module

    cached = render "module.html"
    assert module == cached

    template = File.expand_path("../../fixtures/templates/module.html.eex", __FILE__)

    try do
      File.touch!(template, { { 2030, 1, 1 }, { 0, 0, 0 } })
      not_cached = render "module.html"
      assert module != not_cached
    after
      File.touch!(template, :erlang.universaltime)
    end
  end

  test "uses cached template unless it is cleared" do
    module = render "module.html"
    assert "Elixir-" <> _ = module

    cached = render "module.html"
    assert module == cached

    Dynamo.Templates.Renderer.clear

    not_cached = render "module.html"
    assert module != not_cached
  end

  test "compiles a module with the given templates" do
    Dynamo.Templates.compile_module(CompileTest.CompiledTemplates, @path_finder.all, [:conn], prelude)

    path     = File.join(@fixture_path, "hello.html.eex")
    template = CompileTest.CompiledTemplates.find "hello.html"

    assert Dynamo.Template[identifier: ^path, key: "hello.html",
      handler: Dynamo.Templates.EEXHandler, format: "html",
      ref: { CompileTest.CompiledTemplates, _ }] = template

    { mod, fun } = template.ref
    assert apply(mod, fun, [[], nil]) == { [nil], "HELLO!" }
  end

  defp render(query) do
    { [nil], body } =
      Dynamo.Templates.render Dynamo.Templates.find!(query, [@path_finder]), [conn: nil], [], prelude
    body
  end

  defp prelude do
    fn -> quote(do: use Dynamo.Helpers) end
  end
end