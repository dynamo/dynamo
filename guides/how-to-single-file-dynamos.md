## How to create single file Dynamos

Although Dynamos usually run along an OTP application, they can also be defined in a single file, useful for examples or scripting something quick. The smallest single file Dynamo can be defined as follow:

```elixir
defmodule HelloWorld do
  use Dynamo
  use Dynamo.Router

  config :server, port: 3030

  get "/" do
    conn.resp_body("Hello World!")
  end
end

HelloWorld.start_link
HelloWorld.run
```

The example above defines a module `HelloWorld` which is a `Dynamo` and a `Dynamo.Router`. Then we proceed to define the port and a default route.

After the module is defined, we start the supervisor tree with `HelloWorld.start_link` and run the Dynamo with `HelloWorld.run`.

When using single file Dynamos, some features like code reloading does not work. So they may as well be disabled too by adding:

```elixir
config :dynamo, compile_on_demand: false
```

For a ready to use example, check [examples/hello_world.exs](../examples/hello_world.exs).
