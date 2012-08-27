defmodule Dynamo do
  @doc """
  Starts the Dynamo framework.
  """
  def start do
    :application.start(:mimetypes)
    :application.start(:crypto)
    :application.start(:dynamo)
  end

  # This is private API until we figure out how the
  # application is going to be booted. Booting an app
  # requires:
  #
  # 1. Booting dynamo
  # 2. Booting the app (reloader and stuff)
  # 3. Booting the web app
  #
  # And we will probably want to start a supervisor
  # chain somewhere down the road.
  @doc false
  def start_app(app) do
    if app.config[:dynamo][:compile_on_demand] do
      Dynamo.Reloader.start_link app.config[:dynamo][:source_paths]
    end
  end

  @doc """
  Sets the Dynamo application.
  """
  def app(nil) do
    :application.set_env(:dynamo, :app, nil)
  end

  def app(app) do
    if current = app() do
      raise "Cannot load Dynamo application #{inspect app}, because #{inspect current} is already loaded"
    else
      :application.set_env(:dynamo, :app, app)
    end
  end

  @doc """
  Retrieves the current Dynamo application.
  Returns nil if none is set.
  """
  def app do
    case :application.get_env(:dynamo, :app) do
      { :ok, val } -> val
      :undefined   -> nil
    end
  end

  @doc """
  Reads the current environment. It is the same
  as Mix.env
  """
  def env do
    Mix.env
  end
end