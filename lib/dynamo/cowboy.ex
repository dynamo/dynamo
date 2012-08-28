defmodule Dynamo.Cowboy do
  @moduledoc """
  Provides a runner using Cowboy webserver.
  Check `run/2` for more information.
  """

  @doc """
  Runs the given app with the given options:

  * :port - the port to run the server
  * :acceptors - the number of acceptors for the listener
  * :handler - the Cowboy handler to be used
  * :dispatch - the Cowboy HTTP Dispatch info to be used

  ## Example

      Dynamo.Cowboy.run MyApp, port: 80

  """
  def run(app, options // []) do
    :application.start(:cowboy)

    port      = Keyword.get options, :port, 4000
    acceptors = Keyword.get options, :acceptors, 100
    handler   = Keyword.get options, :handler, Dynamo.Cowboy.Handler
    dispatch  = Keyword.get options, :dispatch, dispatch_for(app, handler)
    verbose   = Keyword.get options, :verbose, true

    port      = to_i(port)
    acceptors = to_i(acceptors)
    options   = Enum.reduce [:port, :acceptors, :handler, :verbose], options, Keyword.delete(&2, &1)
    options   = Keyword.put options, :dispatch, dispatch

    :cowboy.start_listener(app, acceptors,
      :cowboy_tcp_transport, [port: port],
      :cowboy_http_protocol, options
    )

    if verbose, do:
      IO.puts "Running #{inspect app} on port #{port} with Cowboy"
  end

  def shutdown(app) do
    :cowboy.stop_listener(app)
  end

  defp dispatch_for(app, handler) do
    [{ :_, [ {:_, handler, app } ] }]
  end

  defp to_i(integer) when is_integer(integer), do: integer
  defp to_i(binary)  when is_binary(binary), do: binary /> binary_to_list /> list_to_integer
end
