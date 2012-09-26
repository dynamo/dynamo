defmodule Dynamo.Cowboy do
  @moduledoc """
  Provides a runner using Cowboy webserver.
  Check `run/2` for more information.
  """

  @doc """
  Runs the given app with the given options:

  * :port - the port to run the server
  * :acceptors - the number of acceptors for the listener
  * :dispatch - the Cowboy HTTP Dispatch info to be used

  ## Example

      Dynamo.Cowboy.run MyApp, port: 80

  """
  def run(app, options // []) do
    :application.start(:ranch)  
    :application.start(:cowboy)

    port      = options[:port]      || 4000
    acceptors = options[:acceptors] || 100
    dispatch  = options[:dispatch]  || dispatch_for(app)

    port      = to_i(port)
    acceptors = to_i(acceptors)

    unless options[:verbose] == false do
      IO.puts "Running #{inspect app} at localhost:#{port} with Cowboy on #{Dynamo.env}"
    end

    :cowboy.start_http(app, acceptors, [port: port], [dispatch: dispatch])
  end

  def shutdown(app) do
    :cowboy.stop_listener(app)
  end

  defp dispatch_for(app) do
    [{ :_, [ {:_, Dynamo.Cowboy.Handler, app } ] }]
  end

  defp to_i(integer) when is_integer(integer), do: integer
  defp to_i(binary)  when is_binary(binary), do: binary /> binary_to_list /> list_to_integer
end
