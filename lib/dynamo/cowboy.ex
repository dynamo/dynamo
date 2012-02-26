defmodule Dynamo::Cowboy do
  @moduledoc """
  Provides a runner using Cowboy webserver.
  Check  run/2 for more information.
  """

  @doc """
  Runs the given app with the given options:

  * :port - the port to run the server
  * :acceptors - the number of acceptors for the listener
  * :handler - the Cowboy handler to be used
  * :dispatch - the Cowboy HTTP Dispatch info to be used

  ## Example
  
      Dynamo::Cowboy.run MyApp, port: 80
  
  """
  def run(app, options // []) do
    :application.start(:cowboy)

    port      = Orddict.get options, :port, 3000
    acceptors = Orddict.get options, :acceptors, 100
    handler   = Orddict.get options, :handler, Dynamo::Cowboy::Handler
    dispatch  = Orddict.get options, :dispatch, dispatch_for(app, handler)

    IO.puts "Running #{app} on port #{port} with Cowboy"

    options = Enum.reduce [:port, :acceptors, :handler], options, Orddict.delete(&2, &1)
    options = Orddict.put options, :dispatch, dispatch

    :cowboy.start_listener(app, acceptors,
      :cowboy_tcp_transport, [port: port],
      :cowboy_http_protocol, options
    )
  end

  @doc """
  Provides a Cowboy HTTP dispatcher based on the app
  and the handler. A developer can provide his custom
  handler in case he wants to customize the HTTP handling
  mechanism.
  """
  def dispatch_for(app, handler) do
    [{ :_, [ {:_, handler, app } ] }]
  end
end