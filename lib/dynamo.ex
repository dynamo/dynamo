defmodule Dynamo do
  @doc """
  Starts the Dynamo framework.
  """
  def start(env) when is_atom(env) do
    :application.start(:mimetypes)
    :application.start(:crypto)
    :application.start(:dynamo)
    :application.set_env(:dynamo, :env, env)
    :application.set_env(:dynamo, :under_test, nil)
  end

  @doc """
  Reads the current environment.
  """
  def env do
    case :application.get_env(:dynamo, :env) do
      { :ok, env } -> env
      :undefined   -> raise "Dynamo was not started, please invoke Dynamo.start before proceeding"
    end
  end

  @doc """
  Gets the Dynamo used by default under test.
  """
  def under_test() do
    { :ok, mod } = :application.get_env(:dynamo, :under_test)
    mod
  end

  @doc """
  Sets the Dynamo to be used under test.
  """
  def under_test(mod) do
    :application.set_env(:dynamo, :under_test, mod)
  end
end