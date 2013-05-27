## How to use Dynamo with SSL

Using Dynamo with SSL is easy. Dynamo by default uses a web server called `cowboy` which relies on on [Erlang `ssl` application](http://www.erlang.org/doc/man/ssl.html) to provide HTTPS access. That said, the first step is to add the `ssl` application and its dependencies to the `mix.exs` file. Open it up and edit the `application` function to the following:

```elixir
def application do
  [ applications: [:crypto, :public_key, :ssl, :cowboy, :dynamo],
    mod: { PROJECT, [] } ]
end
```

Now all we need to do is to configure SSL. Open up the development configuration at `lib/PROJECT/environments/dev.exs` and add the following:

```elixir
config :ssl,
  port: 4001,
  keyfile: "priv/ssl/key.pem",
  certfile: "priv/ssl/cert.pem",
  password: "cowboy"
```

In this particular example, we are using the [key.pem](../examples/ssl/key.pem) and [cert.pem](../examples/ssl/cert.pem) inside the [examples](../examples/ssl) directory. Please copy both files to the `priv/ssl` directory inside your project.

Run `mix server` and you should be able to access `https://localhost:4001`, after bypassing the SSL warnings from your browser. And that is it!

Before we go, a couple notes:

1. For production, it is likely the SSL certificates won't be in the repository but directly in the deploy machines. You can point to them directly simply by using full paths:

    ```elixir
    config :ssl,
      port: 443,
      keyfile: "/var/www/key.pem",
      certfile: "/var/www/cert.pem",
      password: "..."
    ```

2. Your SSL configuration may require other options. Check the section "SSL OPTION DESCRIPTIONS" in the [Erlang `ssl` application](http://www.erlang.org/doc/man/ssl.html) for descriptions and other options.

3. Currently, there isn't a way to enforce SSL only access to the web application. In case this feature is desired, please open up an issue or a pull request to add this feature.

For a ready to use example, check [examples/ssl.exs](../examples/ssl.exs).