# Run this app from Dynamo root with:
#
#     mix run -r examples/websocket.exs --no-halt
#
# Cowboy Websocket Handler resources:
#
#   * User guide: http://ninenines.eu/docs/en/cowboy/HEAD/guide/ws_handlers
#   * Reference:  http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_websocket_handler
#
defmodule Websocket do
  defmodule Teller do
    def start, do: :global.register_name(:teller, spawn(__MODULE__, :listen, []))
    def server, do: :global.whereis_name(:teller)
    def register(pid), do: server <- { :register, pid }
    def tell(message), do: server <- { :tell, message }
    def listen(registrees // []) do
      receive do
        { :register, pid } -> listen([ pid | registrees ])
        { :tell, message } ->
          Enum.each registrees, fn pid ->
            pid <- { :message, message }
          end
          listen(registrees)
      end
    end
  end

  defmodule WebsocketHandler do
    @behaviour :cowboy_websocket_handler
    def init({:tcp, :http}, _req, _opts), do: { :upgrade, :protocol, :cowboy_websocket }
    def websocket_init(_transport_name, req, _opts) do
      Teller.register(self)
      { :ok, req, :no_state }
    end
    def websocket_handle( { :text, message }, req, state) do
      { :reply, {:text, message }, req, state }
    end
    def websocket_info({ :message, message }, req, state) do
      { :reply, { :text, message }, req, state }
    end
    def websocket_terminate(_reason, _req, _state), do: :ok
  end

  defmodule Router do
    use Dynamo
    use Dynamo.Router

    config :dynamo, compile_on_demand: false
    config :server,
      port: 3030,
      dispatch: [{ :_, [
        {"/websocket", WebsocketHandler, [] },
        {:_, Dynamo.Cowboy.Handler, __MODULE__ }
      ] }]

    get "/*segments" do
      message = Enum.join(conn.route_params[:segments], " ")
      Teller.tell(message)
      conn.resp_body("message sent: #{message}")
    end
  end
end

Websocket.Teller.start
Websocket.Router.start_link
Websocket.Router.run
