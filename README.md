# Dynamo

[![Build Status](https://travis-ci.org/dynamo/dynamo.png?branch=master)](https://travis-ci.org/dynamo/dynamo)

Run, Dynamo, Run!

Dynamo is a web framework that runs on [Elixir](http://elixir-lang.org/). It leverages the power of the Erlang VM to build highly performant and concurrent web applications. Dynamo's goals are performance, robustness and simplicity.

**WARNING:** Dynamo is currently alpha-software and its API will suffer major changes. The current version is an experiment that showcases Elixir's flexibility for building frameworks and its excellent performance.

Dynamo shows excellent performance out of the box, beating similar frameworks like Sinatra and Express, available in other languages, in a callback-free programming fashion. By using the Erlang VM, all of the network I/O is asynchronous but your code appears to be synchronous. The Erlang VM also allows you to use all cores available by default, without a need to start many instances of your web server, and performs well under heavy load with many concurrent open connections.

On the developer side, Dynamo focuses on simplicity by shipping with a bare stack, allowing a team to get started quickly while making it easy to extend the application as and when they see fit.

It is currently alpha software and it supports:

* Light-weight connections with streaming.
* Code organization in environments.
* Code reloading for fast development cycles.
* Lazy parsing and handling of cookies, sessions, params and headers.
* Handling of static assets.
* Template rendering.
* An exceptional exception handler for development.
* Integration with Erlang OTP applications.

Before becoming beta, we want to add the following to Dynamo:

* Logging.
* Websockets support.
* Built-in JSON encoding.
* Database adapters.

This README will go into Dynamo installation and a basic walk-through.

# Installation

As an alpha-software, Dynamo installation is a bit manual but can be done in few steps:

1. Ensure you are on Elixir master (available on homebrew, see below)

2. Clone this repository and go to its directory

3. Get Dynamo dependencies and run tests with: `MIX_ENV=test mix do deps.get, test`

4. Create a project: `mix dynamo path/to/your/project`

Congratulations! You created your first project with Dynamo! Let's run it:

1. Go to your app

2. Get dependencies with: `mix deps.get`

3. Run it: `iex -S mix server`

If you do not want a console, then start the app with `mix server`.

Check the `lib/` and `web/` folders for more information. Changes done in the `web` directory are picked up without a need to reload the server.

Static content is served from `priv/static/` folder and from the `/static` route.

Your project can be compiled and used in production with: `MIX_ENV=prod mix do compile, server`.

## Installation using the **master** branch of elixir

Elixir evolves rapidly and Dynamo follows it. If you get build or test errors, please use the
current **master** branch of elixir before reporting any bugs.

If you're on OSX and use [homebrew](http://mxcl.github.io/homebrew/), this is easily done as follows:

```bash
    $ brew unlink elixir # remove non-head installation of elixir
    $ brew install elixir --HEAD
```

# Walk-through

This walk-through is going to put you in touch with the four main elements in Dynamo: routers, connection, OTP and the Dynamo itself. This walk-through requires you to be familiar with Elixir, so please read [Elixir's getting started guide](http://elixir-lang.org/getting_started/1.html) if you haven't yet.

## Routers

A Dynamo is organized in routers. Routers are kept in the `web/routers` directory and by default a project contains an `ApplicationRouter` defined at `web/routers/application_router.ex`, which is your Dynamo main entry point:

```elixir
defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn.fetch([:cookies, :params])
  end

  # It is common to break your Dynamo in many
  # routers forwarding the requests between them
  # forward "/posts", to: PostsRouter

  get "/" do
    conn = conn.assign(:title, "Welcome to Dynamo!")
    render conn, "index.html"
  end
end
```

All routers must use the `Dynamo.Router` module. By using this module you have access to the macros `get`, `post`, `put`, `patch` and `delete` that allows developers to generate routes. Here is a simple route:

```elixir
get "/hello/world" do
  conn.resp(200, "Hello world")
end
```

The `conn` value above represents a connection, about which we are going to get into more details soon. In the example above in particular, we are setting the connection to have response status `200` with the body `"Hello world"`. Routes can also contain dynamic segments:

```elixir
put "/users/:user_id" do
  conn.resp 200, "Got user id: #{conn.params[:user_id]}"
end
```

```elixir
get "/hello/*" do
  conn.resp 200, "Match all routes starting with /hello"
end
```

Each route is compiled down to a function clause, making route matching extremely fast and also allows the use of guard constraints:

```elixir
get "/section/:section" when section in ["contact", "about"] do
  render conn, "#{section}.html"
end
```

In general, Dynamos are broken into many routers, with requests forwarded between them. Furthermore, routers also provide hooks, such as the `prepare` hook seen above.

### Hooks

Hooks are a mechanism to encapsulate actions that are shared between many routes. For example, one may write a hook that only allows requests with a given parameter to proceed:

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

Besides adding hooks to a router as a whole, hooks can also be added to a specific route using the `@prepare` and `@finalize` attributes before each route:

```elixir
@prepare :authenticate_user
get "/users/:user_id" do
  # ...
end

defp authenticate_user(conn) do
  unless get_session(conn, :user_id) do
    halt! conn.status(401)
  end
end
```

### Forwarding

As your Dynamo grows, it would be impractical to have all routes in a single router. For this reason, Dynamo allows you to easily compose and forward requests between routers. Let's consider the router generated by default:

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

In the example above, all routes starting with `/posts` will be automatically routed to the `PostsRouter`. Furthermore, the `PostsRouter` will receive on the trailing part of the url. A request to `/posts/recent` will be seen by the `PostsRouter` as `/recent`:

```elixir
defmodule PostsRouter do
  use Dynamo.Router

  get "/recent" do
    # Get all recent posts
  end
end
```

Finally, in the `ApplicationRouter` above, notice we fetch cookies and the request params for every request. However, if just the `PostsRouter` is using such information, the `prepare` hook could be moved to inside the `PostsRouter`, so it will be fetched only when it is needed.

This forwarding mechanism makes it very easy to split and organize your code into its logical sections while also having control on which features are required by each router.

## Connection

The connection plays a huge part when building Dynamos. As we have seen above, each route has access to the connection as the **variable** `conn`. The `authenticate_user` prepare hook defined above also receives the connection as an argument:

```elixir
defp authenticate_user(conn) do
  unless get_session(conn, :user_id) do
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

Elixir data structures are immutable. If you have a tuple, you can't modify this tuple in place. Adding or removing elements will actually return a new tuple:

```elixir
first  = { 1, 2, 3 }
second = set_elem(first, 0, :hello)

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

Almost all functions throughout Dynamo will receive and **return a connection**. For example, the `render` function above will return a new connection with the render template as a body. Similarly, all routes and hooks must return the connection, you will get a gentle exception if you don't.

### APIs

The connection data structure keeps the low-level API to interact with web servers. You can set status codes and response bodies, handle headers and parse request information as `:params`, `:cookies`, `:session` and so forth.

Furthermore, the connection is responsible to carry out the information in between routers and between routers and templates via assigns. For example:

```elixir
get "/" do
  conn = conn.assign(:title, "Welcome to Dynamo!")
  render conn, "index.html"
end
```

A `prepare` hook may also set the a `current_user` assign which could then be retrieved in any router as `conn.assigns[:current_user]`. You can find more information about assigns and other connection functions in [Dynamo.Connection](http://elixir-lang.org/docs/dynamo/Dynamo.Connection.html).

Dynamo also builds many functionalities on top of this low-level connection API:

* [Dynamo.HTTP.Cookies](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Cookies.html) - conveniences for working with cookies
* [Dynamo.HTTP.Halt](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Halt.html) - conveniences for halting a connection, as the function `halt!` we saw in some examples
* [Dynamo.HTTP.Redirect](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Redirect.html) - conveniences for redirecting a connection
* [Dynamo.HTTP.Render](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Render.html) - conveniences for rendering templates
* [Dynamo.HTTP.Session](http://elixir-lang.org/docs/dynamo/Dynamo.HTTP.Session.html) - conveniences for working with session

All those functions in `Dynamo.HTTP.*` are imported by default into your `Dynamo.Router`.

### Templating

Dynamo templates uses [EEx](http://elixir-lang.org/docs/stable/EEx.html) to
let you embed Elixir logic into your HTML templates. For example, if you had
this route:

```elixir
get "/fruits" do
  conn = conn.assign(:fruits, [{0, "Apple"}, {1, "Orange"}, {2, "Banana"}])
  render conn, "fruits.html"
end
```

To display the list of fruits, `fruits.html.eex` might look like:

```
<html>
<body>
  <ul>
  <%= lc { id, name } inlist @fruits do %>
    <li><a href="/fruit/<%= id %>"><%= name %></a></li>
  <% end %>
  </ul>
</body>
</html>
```

Notice we have used list comprehensions but any of the `Enum` functions,
like `Enum.filter/2` and `Enum.map/2` are also available.

## OTP application

The next two sections will focus on the Dynamo integration with OTP, which is a set of tools and libraries for building robust, fault-tolerant applications. Talking about "applications", you may have noticed throughout this tutorial we haven't used the word "application" a lot so far. This is intentional as there is no such thing as a "Dynamo application"!

I will explain. When we first invoked `mix dynamo path/to/your/project`, it generated a **project**. This project may contain one or more OTP applications (by default, just one) and the Dynamo is always part of an **OTP application**.

In this section we are going to detail what it means to be an OTP application. Before continuing, make sure you have the guide about [building OTP apps with Mix](http://elixir-lang.org/getting_started/mix/2.html), because we will build on top of it.

### Getting the names right

To recap, when we invoke `mix dynamo path/to/your/project`, we have:

* a **project** - the project is everything that was generated and its backbone is the `mix.exs` file that contains the project dependencies, tasks to compile and run tests and much more;

* one or more **applications** - a project may contain one or more application, but most of the cases, it contains just one. In general, an application is an artifact generated by a project. For example, when you compile your code, you will have a bunch of `.beam` files and an `.app` file inside the `ebin` directory. The `priv` directory is also part of the application and it contains everything that is needed at runtime but is not source code, like assets, database configuration, etc. Your application also has dependencies and they are a subset of your project dependencies (in general, test and compile-time dependencies are not application dependencies);

* one or more **dynamos** - one application may contain one or more dynamos. Yes, you heard it right: Dynamo was designed from the ground up to be isolated, so you can have many Dynamos, listening to and serving different ports in the same project and running in the same OS process.

### Applications and supervisors

A project generated with `mix dynamo` contains the same elements as a basic OTP application. Assume we generate the same project as we did in [building OTP apps with Mix](http://elixir-lang.org/getting_started/mix/2.html) but now using the dynamo command:

    $ mix dynamo stacker

In the `mix.exs` file, we will see the project definition, containing the main application, the version and the project dependencies. We will also see a function named `application`, this function returns the application specification:

```elixir
def application do
  [ applications: [:cowboy, :dynamo],
    mod: { Stacker, [] } ]
end
```

This **application specification** declares it depends on two other applications, in this case `:cowboy` (the web server) and `:dynamo` (this web framework). It also specifies an **application callback**. The application callback is the module `Stacker` and it will be initialized receiving an empty list as the argument.

If we open up the `lib/hello.ex` file, we can see the application callback implementation:

```elixir
defmodule Stacker do
  use Application.Behaviour

  @doc """
  The application callback used to start this
  application and its Dynamos.
  """
  def start(_type, _args) do
    Stacker.Dynamo.start_link([max_restarts: 5, max_seconds: 5])
  end
end
```

As explained in the [building OTP apps with Mix guide](http://elixir-lang.org/getting_started/mix/2.html), the application callback must return the PID of an OTP Supervisor. In this case, we are returning the PID of the Dynamo supervisor. In this case, if our Dynamo fails, the Erlang VM will act on it.

In case your application has other workers and supervisors, you can create your own supervisor tree and simply embed the Dynamo supervisor in your own tree. For example, imagine we copy both `Stacker.Supervisor` and `Stacker.Server` from the Mix guide, the supervisor looks like this:

```elixir
defmodule Stacker.Supervisor do
  use Supervisor.Behaviour

  # A convenience to start the supervisor
  def start_link(stack) do
    :supervisor.start_link(__MODULE__, stack)
  end

  # The callback invoked when the supervisor starts
  def init(stack) do
    children = [ worker(Stacker.Server, [stack]) ]
    supervise children, strategy: :one_for_one
  end
end
```

Notice this supervisor has its own worker, called `Stacker.Server`, that should be part of the supervision tree. We can embed the Dynamo supervisor in that tree by changing the `init` function:

```elixir
def init(stack) do
  children = [
    worker(Stacker.Server, [stack]),
    supervisor(Stacker.Dynamo, [])
  ]
  supervise children, strategy: :one_for_one
end
```

Now all we need to do is to change the `Stacker` module to initialize the new supervisor tree instead of the Dynamo directly:

```elixir
defmodule Stacker do
  use Application.Behaviour

  def start(_type, _args) do
    Stacker.Supervisor.start_link([])
  end
end
```

This shows that your application is the one in **control** and a Dynamo is simply embedded into it.

## Dynamo

So what is a Dynamo after all? A Dynamo is nothing more than a module which can be found at `lib/*/dynamo.ex` of a newly generated project. Here is an example:

```elixir
defmodule Stacker.Dynamo do
  use Dynamo

  config :dynamo,
    env: Mix.env,
    otp_app: :stacker,
    endpoint: ApplicationRouter,
    static_route: "/static"
end
```

When you run `mix compile`, this Dynamo is compiled and becomes functional when you start your application supervision tree, as we just discussed. The Dynamo is responsible for handling requests, compiling and managing the files under the `web` directory, and each Dynamo also has its own supervisor tree.

Finally, notice that in the same directory you find your Dynamo you can also find an `environments` directory which contains configuration specific for each environment. Take a look at them for examples on the available configuration options.

Summing up, when you run `mix dynamo stacker` to create a project and then fetch its dependencies, this is the final structure you get:

    - deps                        # Your project dependencies
    - lib                         # Contains your application files
      - lib/stacker.ex            # Your application callback
      - lib/stacker/dynamo.ex     # The Dynamo definition
      + lib/stacker/environments  # Dynamo environment related configuration
    - mix.exs                     # Your project specification
    - priv                        # Application files that are needed at runtime
      + priv/static               # Static assets (they are needed on runtime!)
    - test                        # Your test files
      + test/features             # End-to-end tests
      + test/routers              # Routers unit tests
    - web                         # Files managed and compiled by the Dynamo
      + web/routers               # Your application routers

That's all. If you haven't built an OTP application before, you may be a bit overwhelmed but there is nothing stop you from diving into the `web` directory and learning about OTP just when you need it. Before you know it, you will be leveraging the power of the Erlang VM to build robust, high performance and concurrent web applications!

## Learn more

In the [guides](guides) directory we include a bunch of small, simple guides that teach you how to achieve something in Dynamo: 

* [How to create single file Dynamos](guides/how-to-single-file-dynamos.md), (although not recommended for production, the [examples](examples) directory usually uses single file Dynamos to easily show how to achieve something)

* [How to run a Dynamo on Heroku](guides/how-to-heroku.md)

* [How to render templates with layouts](guides/how-to-layouts.md)

* [How to import functions (helpers) into templates](guides/how-to-helpers.md)

* [How to use SSL](guides/how-to-ssl.md)

# License

Dynamo source code is released under Apache 2 License.
Check LICENSE file for more information.
