# Dynamo

Run, Dynamo, Run!

Dynamo is a web framework that runs on [Elixir](http://elixir-lang.org/). Its goals is performance and simplicity. It is currently alpha software and it supports:

* Light-weight connections with streaming
* Code organization in environments
* Code reloading for fast development cycles
* Lazy parsing of cookies, sessions and headers
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

The application can be compiled and used in production with: `MIX_ENV=prod mix do compile, server`. This is also the best environment to benchmark Dynamo and see its amazing results. ;)

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

#### Hooks

TODO

#### Forwarding

TODO

## Connection

TODO

## OTP support

TODO

## Dynamo

TODO

# Learn more

TODO

# License

Dynamo source code is released under Apache 2 License.
Check LICENSE file for more information.
