# Dynamo

Run, Dynamo, Run!

Dynamo is a web framework that runs on [Elixir](http://elixir-lang.org/). It leverages the power of the Erlang VM to build highly performand and concurrent web applications.

Dynamo goals are performance and simplicity. It provides a bare stack with the minimum required for a team to be productive but also allows them to easily include features as and when they see fit.

It is currently alpha software and it supports:

* Light-weight connections with streaming
* Code organization in environments
* Code reloading for fast development cycles
* Lazy parsing and handling of cookies, sessions, params and headers
* Handling of static assets
* Template rendering
* Integration with Erlang OTP applications

Before becoming beta, we want to add the following to Dynamo:

* Websockets support
* Built-in JSON encoding
* Database adapters

This README will go into Dynamo installation and a basic walk-through.

Documentation for Dynamo is available at [elixir-lang.org/docs/dynamo](http://elixir-lang.org/docs/dynamo).

# Installation

As an alpha-software, Dynamo installation is a bit manual but can be done in few steps:

1. Ensure you are on Elixir v0.8.1 and you have [rebar](https://github.com/basho/rebar) (available on homebrew) installed

2. Clone this repository and go to its directory

3. Get Dynamo dependencies and run tests with: `MIX_ENV=test mix do deps.get, test`

4. Create a project: `mix dynamo path/to/your/app`

Congratulations! You created your first Dynamo! Let's run it:

1. Go to your app

2. Get dependencies with: `mix deps.get`

3. Run it: `mix server`

Check `lib/` and `web/` folders for more information. Changes done in the `web` directory are picked up without a need to reload the server.

Static content is served from `priv/static/` folder and from the `/static` route.

The application can be compiled and used in production with: `MIX_ENV=prod mix do compile, server`.

# Basic walk-through

This walk-through is going to put you in touch with the four main elements in Dynamo: routers, connection, OTP and the Dynamo itself.

## Routers

A Dynamo application is organized in routers. Routers are kept in the `web/routers` directory and by default a project contains an `ApplicationRouter`, which is your Dynamo main entry point:

```elixir
defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn.fetch([:cookies, :params])
  end

  # It is common to break your application in many
  # routers forwarding the requests between them
  # forward "/posts", to: PostsRouter

  get "/" do
    conn = conn.assign(:title, "Welcome to Dynamo!")
    render conn, "index.html"
  end
end
```

All routers must use the `Dynamo.Router` module. By using this module, we have access to the macros `get`, `post`, `put` and `delete` that allows developers to generate routes. Here is a simple route:

```elixir
get "/hello/world" do
  conn.resp(200, "Hello world")
end
```

The `conn` value above represents a connection which we are going to get into more details soon. In the example above in particular, we are setting the connection to have response status 200 with the body `"Hello world"`. Routes can also contain dynamic segments:

```elixir
put "/users/:user_id" do
  conn.resp 200, "Got user id: #{conn.params[:user_id]}"
end
```

Each route is compiled down to a function clause, this makes routes matching extremely fast and also allow the use of guard constraints:

```elixir
get "/section/:section" when section in ["contact", "about"] do
  render conn, "#{section}.html"
end
```

In general, Dynamos are broken into many routers which forwards the request in between them. Furthermore, routers also provide hooks, as the `prepare` hook seen above.

### Hooks

Hooks are a mechanism to encapsulate actions that are shared in between many routes. For example, one may write a hook that only allow requests with a given parameter to proceed:

```elixir
prepare do
  unless conn.params[:user_info] do
    halt! conn.status(400)
  end
end
```

Another common functionality in hooks is to prepare the connection, sometimes by setting a layout:

```elixir
prepare do
  conn.assign :layout, "home.html"
end
```

Or by fetching information used throughout all routes:

```elixir
prepare do
  conn.fetch :params
end
```

There is also a `finalize` hook which is used after the request is processed.

Besides adding hooks to a router as whole, hooks can also be added per specific route using the `@prepare` and `@finalize` attributes before each route:

    @prepare :authenticate_user
    get "/users/:user_id" do
      # ...
    end

    defp authenticate_user(conn) do
      unless conn.session[:user_id] do
        halt! conn.status(401)
      end
    end

### Forwarding

As your application grows, it would be impractical to have all routes in a single router. For this reason, Dynamo allows you to easily compose and forward requests in between routers. Let's consider the router generated by default in an application:

```elixir
defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    conn.fetch([:cookies, :params])
  end

  forward "/posts", to: PostsRouter

  get "/" do
    conn = conn.assign(:title, "Welcome to Dynamo!")
    render conn, "index.html"
  end
end
```

In the example above, all routes starting with `/posts` will be automatically routed to the `PostsRouter`. Furthermore, the `PostsRouter` will receive on the trailing part of the url. For instance, a request to `/posts/recent`, it will be seen by the `PostsRouter` as `/recent`:

```elixir
defmodule PostsRouter do
  use Dynamo.Router

  get "/recent" do
    # Get all recent posts
  end
end
```

Finally, in the `ApplicationRouter` above, notice we fetch cookies and the request params for every request. However, if just the `PostsRouter` is using such information, the `prepare` hook could be moved to inside the `PostsRouter`, so it will be fetched only when it is needed.

This forwarding mechanism makes it very easy to split and organize your code by its logical sections while also having control on which features is required by each router.

## Connection

The connection plays a huge part when building Dynamos. As we have seen above, each route have access to the connection as the **variable** `conn`. The `authenticate_user` prepare hook defined above also receives the connection as argument:

```elixir
defp authenticate_user(conn) do
  unless conn.session[:user_id] do
    halt! conn.status(401)
  end
end
```

The connection is a **direct interface to the underlying web server**. In the examples above, we have used `conn.resp(status, body)` to set up our response. This will set a status and a response that will be sent back to the client **after** the route finishes processing.

However, a developer could also use `conn.send(status, body)`, which will send the response immediately:

```elixir
conn.send(200, "Hello world")
```

In the example above there is no wait and the response is sent immediately. The fact the connection is a direct interface makes streaming equally trivial:

```elixir
conn = conn.send_chunked(200)
conn.chunk("Hello World")
```

There are no dirty hacks nor work-around required. You get a fast and clean API to interact with the web server.

### Immutability

One common confusion for developers starting with Dynamo (and to certain extent functional programming too) is immutability.

Elixir data structures are immutable, this means that, if you have a tuple, you can't modify this tuple in place. Adding or removing elements will actually return a new tuple:

```elixir
first  = { 1, 2, 3 }
second = setelem(first, 0, :hello)

first  #=> { 1, 2, 3 }
second #=> { :hello, 2, 3 }
```

Notice how the elements of the first tuple remain intact. The same happens with the connection data structure. Consider this route:

```elixir
get "/" do
  conn.assign(:title, "Welcome to Dynamo!")
  render conn, "index.html"
end
```

The first line `conn.assign(:title, "Welcome to Dynamo!")` has no effect whatsoever because we don't store the result of the operation anywhere. In fact, if someone writes `IO.inspect conn.assigns[:title]` in the next line, the value of the `:title` assign will still be `nil`. That said, as we do with tuples, we need to store the new connection in the updated variable:

```elixir
get "/" do
  conn = conn.assign(:title, "Welcome to Dynamo!")
  render conn, "index.html"
end
```

Almost all functions through Dynamo will receive and **return a connection**. For example, the `render` function above will return a new connection with the render template as a body. Similarly, all routes and hooks must return the connection, you will get a gentle exception if you don't.

### APIs

The connection data structure keeps the low-level API to interact with web servers. You can set status codes and response bodies, handle headers and parse request information as `:params`, `:cookies`, `:session` and so forth. You can find more information about the connection functions in [Dynamo.Connection](http://elixir-lang.org/docs/dynamo/Dynamo.Connection.html).

Dynamo builds many functionalities on top of this low-level API:

* [Dynamo.HTTP.Cookies](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Cookies.html) - conveniences for working with cookies
* [Dynamo.HTTP.Halt](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Halt.html) - conveniences for halting a connection, as the function `halt!` we saw in some examples
* [Dynamo.HTTP.Hibernate](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Hibernate.html) - conveniences for awaiting and hibernating a connection
* [Dynamo.HTTP.Redirect](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Redirect.html) - conveniences for redirecting a connection
* [Dynamo.HTTP.Render](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Render.html) - conveniences for rendering templates
* [Dynamo.HTTP.Session](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Session.html) - conveniences for working with session

All those functions are imported by default into your `Dynamo.Router`.

## OTP support

TODO

## Dynamo

TODO

## Learn more

TODO

# License

Dynamo source code is released under Apache 2 License.
Check LICENSE file for more information.
