## How to use sessions

Dynamo supports sessions, with very minimal configuration.

### Configuring a session store

Dynamo provides two session stores - `Session.CookieStore` and `Session.ETSStore`.

#### Cookie store

In your project's `dynamo.ex`, add/uncomment the following lines:

```elixir
config :dynamo,
  session_store: Session.CookieStore,
  session_options:
    [ key: "_example_session",
      secret: "YOUR-SECRET-HERE"]
```

Change the key and secret in the session options to suit your needs.

#### ETS store

If you prefer using ETS to store sessions, add the following lines:

```elixir
config :dynamo,
  session_store: Session.ETSStore,
  session_options: [
    table: :dynamo_sessions,
    key: "_example_session"
  ]
```

Change the key in the session options to suit your needs.

You must pass a `table` key in the session options, with the name of an existing ETS table as the value. Use the following snippet to create an ETS table, whereever you see fit.

```elixir
:ets.new(:dynamo_sessions, [:named_table, :public, {:read_concurrency, true}])
```

You can change the name of the sessions table, just make sure to pass the correct name in the session options.


### Fetch the session in your prepare hook

In your application router, change the line

```elixir
conn.fetch([:cookies, :params])
```
to
```elixir
conn.fetch([:cookies, :params, :session])
```

This adds the session data to the fetch list, to fetch and cache it.

### Managing session data

Configuring a session store imports session helper methods into your routers.


#### put_session(conn, key, value)

Adds session data to the connection.

```elixir
put_session(conn, :user_id, "s23fa3aefvtwerg")
```

#### get_session(conn, key)

Gets session data for a specific key.

```elixir
get_session(conn, :user_id)
```

You can also get all the session data in one call.

```elixir
get_session(conn)
```

#### delete_session(conn, key)

Deletes a key from the session data.

```elixir
delete_session(conn, :user_id)
```

#### configure_session(conn, key, value)

Configures the session.

```elixir
# sets path of the session
configure_session(conn, :path, "/")

# sets the session as secure
configure_session(conn, :secure, true)
```
