## How to render templates with layouts

Dynamo supports layout rendering in routers. All you need to do is to assign the layout you want to render:

```elixir
defmodule SampleRouter do
  use Dynamo.Router

  prepare do
    conn.assign :layout, "main"
  end

  get "/" do
    render conn, "hello.html"
  end
end
```

Notice that the rendering a template requires the extension, in this case, `hello.html` but setting a layout does not. This is because the layout extension is retrieved from the rendered template.

The layout should be placed in your templates path, usually at `web/templates/layouts/main.html.eex`:

```erb
<!DOCTYPE HTML>
<html>
<head>
  <title>Title</title>
</head>
<body><%= content_for(:template) %></body>
</html>
```

We use the `content_for(:template)` macro to retrieve the content of the template and print it inside the layout. The `hello.html` template should be placed at `web/templates/hello.html.eex` with, for example, the following contents:

```erb
Hello from template!
```

### Passing data in between templates

Sometimes you want to exchange more information in between templates than the template body. For such, you can also `content_for` similar to how we have seen above. Imagine we want to allow the template to set the page title but still allow a default value. We could change our layout to the following:

```erb
<!DOCTYPE HTML>
<html>
<head>
  <title><%= content_for(:title) || "Title" %></title>
</head>
<body><%= content_for(:template) %></body>
</html>
```

And in the template we could write:

```erb
<% content_for :title do %>
  Title from template
<% end %>

Hello from template!
```

### Rendering templates

Finally, sometimes it may be convenient to split a template into smaller chunks. Inside templates, there is a `render` macro available that allows you to render other templates:

```erb
Hello from template!
<%= render "another.html" %>
```

Assuming you have a file named `web/templates/another.html.eex`, it will also be rendered and printed in the final template.

For a ready to use example, check [examples/layouts.exs](../examples/layouts.exs).

### Other helpers

Both `content_for` and `render` macros used in this guide are part of Dynamo helpers. Dynamo ships with a couple helpers that make programming templates a bit easier, check the [helpers guide](how-to-helpers.md) for more information.
