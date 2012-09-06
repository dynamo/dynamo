Run, dynamo, run!

## Adventure mode!

If you are felling adventurous, run a Dynamo app in few steps:

1) Ensure you are on Elixir master;

2) Clone this repository and go to its directory;

3) `mix test`

4) `mix dynamo path/to/your/app`

Congratulations! You created your first Dynamo app! Let's run it:

1) Go to your app

2) Get dependencies with: `mix deps.get`

3) Run it: `mix server`

Check `app/` and `config/` folders for more info. Changes done in the app directory are picked up without a need to reload the server!

Public content is served from `public/` folder and from the `/public` route.

Production use is done with: `MIX_ENV=prod mix do compile, server`