## How to import functions (helpers) into templates

Some projects may have a set of functions they would like to share across all templates. Those functions could be easily imported into any template by means of the import directive:

```erb
<% import MyHelpers %>
```

However, including the same module in every template may be a bit cumbersome. For this reason, Elixir allows you define a set of modules to be added as prelude for each template. Open up `lib/PROJECT/dynamo.ex` and you can see the following chunk of code:

```elixir
templates do
  use Dynamo.Helpers
end
```

The code above is adding by default all of `Dynamo.Helpers`. You can import your own modules inside the same chunk of code:

```elixir
templates do
  use Dynamo.Helpers
  import MyHelpers
end
```

Restart the Dynamo and now all functions from MyHelpers will also be available in your templates.

### Dynamo.Helpers

Sometimes, it may be the case Both `content_for` and `render` macros used in this guide are part of Dynamo helpers. Dynamo ships with a couple helpers that make programming templates a bit easier:

* [Dynamo.Helpers.ContentFor](http://elixir-lang.org/docs/dynamo/Dynamo.Helpers.ContentFor.html) - helpers for passing data in between templates
* [Dynamo.Helpers.Escaping](http://elixir-lang.org/docs/dynamo/Dynamo.Helpers.Escaping.html) - helpers for escaping html inside templates
* [Dynamo.Helpers.Rendering](http://elixir-lang.org/docs/dynamo/Dynamo.Helpers.Rendering.html) - helpers for rendering templates inside templates
