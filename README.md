Run, dynamo, run!

## Adventure mode!

If you are feeling adventurous, create a Dynamo in few steps:

1) Ensure you are on Elixir master and you have [rebar](https://github.com/basho/rebar) (available on homebrew) installed

2) Clone this repository and go to its directory

3) Get Dynamo dependencies and run tests with: `MIX_ENV=test mix do deps.get, test`

4) Create a project: `mix dynamo path/to/your/app`

Congratulations! You created your first Dynamo! Let's run it:

1) Go to your app

2) Get dependencies with: `mix deps.get`

3) Run it: `mix server`

Check `app/` and `lib/` folders for more info. Changes done in the app directory are picked up without a need to reload the server!

Static content is served from `priv/static/` folder and from the `/static` route.

Production use is done with: `MIX_ENV=prod mix do compile, server`