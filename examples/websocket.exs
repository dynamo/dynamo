# Run this app from Dynamo root with:
#
#     mix run -r examples/websocket.exs --no-halt
#
# Open a browser supporting websockets at URL http://localhost:3030 and wait for 3 seconds.
#
defmodule Websocket do
  # Cowboy Websocket Handler resources:
  #
  #   * User guide: http://ninenines.eu/docs/en/cowboy/HEAD/guide/ws_handlers
  #   * Reference:  http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_websocket_handler
  #
  defmodule Handler do
    @behaviour :cowboy_websocket_handler
    def init({:tcp, :http}, _req, _opts), do: { :upgrade, :protocol, :cowboy_websocket }
    def websocket_init(_transport_name, req, _opts) do
      :timer.send_interval(3000, { :message, "Are we there yet?" })
      { :ok, req, :no_state }
    end
    def websocket_info({ :message, message }, req, state) do
      { :reply, { :text, message }, req, state }
    end
    def websocket_handle({ :text, message }, req, state) do
      { :reply, { :text, message }, req, state }
    end
    def websocket_terminate(_reason, _req, _state), do: :ok
  end

  defmodule Router do
    use Dynamo
    use Dynamo.Router

    config :dynamo, templates_paths: [ Path.expand("../websocket", __FILE__) ]
    config :server,
      port: 3030,
      dispatch: [{ :_, [
        {"/websocket", Handler, [] },
        {:_, Dynamo.Cowboy.Handler, __MODULE__ }
      ] }]

    get "/" do
      render conn, "index.html"
    end
  end
end

Websocket.Router.start_link
Websocket.Router.run
