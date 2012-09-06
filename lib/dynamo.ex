defmodule Dynamo do
  @doc """
  Starts the Dynamo framework.
  """
  def start(env) when is_atom(env) do
    :application.start(:mimetypes)
    :application.start(:crypto)
    :application.start(:dynamo)
    :application.set_env(:dynamo, :env, env)
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
  Reads the current environment.
  """
  def env do
    { :ok, env } = :application.get_env(:dynamo, :env)
    env
  end
end