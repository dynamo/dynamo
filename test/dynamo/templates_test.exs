defmodule Dynamo.TemplatesTest do
  use ExUnit.Case, async: true

  @renderer __MODULE__.Renderer
  @fixture_path Path.expand("../fixtures/templates", __DIR__)

  setup_all do
    Dynamo.Templates.Renderer.start_link(@renderer)
    :ok
  end

  teardown_all do
    Dynamo.Templates.Renderer.stop(@renderer)
    :ok
  end

  test "escapes wildcard characters" do
    assert nil? Dynamo.Templates.find("hello?html", [@fixture_path])
  end

  test "renders a template" do
    body = render "hello.html"
    assert body == "HELLO!"
  end

  test "renders a normalized template name" do
    body = render "/hello.html"
    assert body == "HELLO!"
  end

  test "uses cached template unless it changes" do
    module = render "module.html"
    assert "Elixir." <> _ = module

    cached = render "module.html"
    assert module == cached

    template = Path.expand("../fixtures/templates/module.html.eex", __DIR__)

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
    assert "Elixir." <> _ = module

    cached = render "module.html"
    assert module == cached

    Dynamo.Templates.Renderer.clear @renderer

    not_cached = render "module.html"
    assert module != not_cached
  end

  test "compiles a module with the given templates" do
    all = Dynamo.Templates.Finder.all(@fixture_path)
    Dynamo.Templates.compile_module(CompileTest.CompiledTemplates, all, [:conn], prelude)

    path     = Path.join(@fixture_path, "hello.html.eex")
    template = CompileTest.CompiledTemplates.find "hello.html"

    assert %Dynamo.Template{identifier: ^path, key: "hello.html",
      handler: Dynamo.Templates.EEXHandler, format: "html",
      ref: { CompileTest.CompiledTemplates, _ }, finder: @fixture_path} = template

    { mod, fun } = template.ref
    assert apply(mod, fun, [[], nil]) == { [nil], "HELLO!" }
  end

  defp render(query) do
    { [nil], body } =
      Dynamo.Templates.render @renderer,
        Dynamo.Templates.find!(query, [@fixture_path]),
        [conn: nil], [], prelude
    body
  end

  defp prelude do
    fn ->
      quote do
        import List, only: [flatten: 1], warn: false
        use Dynamo.Helpers
      end
    end
  end
end
