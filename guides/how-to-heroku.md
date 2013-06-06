# Running on Heroku

You can run Dynamo on Heroku using the [Elixir third-party buildpack](https://github.com/goshakkk/heroku-buildpack-elixir). 
Follow the [steps from the README](https://github.com/elixir-lang/dynamo#installation) to create a Dynamo project, then do the following:

### Create a new app:  

From the project directory, create a new Heroku app and specify the buildpack:

    $ heroku create --buildpack "https://github.com/goshakkk/heroku-buildpack-elixir.git" [app_name]

If you already have a Heroku app created, you can just set the `BUILDPACK_URL` config var:

    $ heroku config:add BUILDPACK_URL="https://github.com/goshakkk/heroku-buildpack-elixir.git" -a YOUR_APP

### Specify Erlang/OTP and Elixir versions

Elixir requires Erlang/OTP version R15 or greater. Specify a preferred Erlang/OTP version for the buildpack by creating a `.preferred_otp_version`:  

    $ echo "OTP_R16B" > .preferred_otp_version
  
By default, the Elixir buildpack uses the master branch version of Elixir. You can specify a custom branch or tag name from the https://github.com/elixir-lang/elixir repository in the `.preferred_elixir_version` dotfile:

    $ echo "master" > .preferred_elixir_version

### Setup a Procfile

Heroku needs a Procfile in order to run your application. Create a Procfile with a `web` process defined:

    $ echo 'web: MIX_ENV=prod mix server -p $PORT' > Procfile
    
**Important Note:** Single quotes are important here. `$PORT` is an environment variable supplied by Heroku. If you use double quotes 
in the above `echo` call, your local shell will try to interpolate the contents, and you'll end up with `-p ` and not `-p $PORT`. 
    
### Deploy

Add and commit your changes, then push to Heroku:

    $ git add Procfile .preferred_otp_version .preferred_elixir_version  
    $ git commit -m "Setup for Heroku"  
    $ git push heroku master
