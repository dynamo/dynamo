defmodule Dynamo.Websocket do
  @moduledoc """
  This module defines the API required to use websockets
  with Dynamo. The first step is to send the connection
  back with upgrade instructions and a handler:

      get "/echo" do
        conn.upgrade(:websocket, EchoSocket)
      end

  The connection will be sent back and the :websocket protocol
  will be initiated. The `EchoSocket` module should implement
  all the callbacks defined below. `Dynamo.Websocket.Behaviour`
  can be used for a default implementation.
  """

  use Behaviour

  @type conn    :: Dynamo.HTTP.t
  @type message :: { :text | :binary | :ping | :pong, binary }
  @type terminate_reason :: { :normal, :closed | :timeout } | { :error, atom }

  @doc """
  Receives the connection and must do any websocket initialization.

  A timeout can be set which is going to be used throughout all
  interactions and terminate the socket with reason
  `{ :normal, :timeout }` in case it times out.
  """
  defcallback init(conn) ::
    { :ok, conn } |
    { :ok, conn, timeout } |
    { :ok, conn, :hibernate } |
    { :ok, conn, timeout, :hibernate } |
    { :shutdown, conn }

  @doc """
  Handles messages received from the socket.
  It can do nothing (`:ok`), reply back to the socket
  or shutdown the connection.
  """
  defcallback handle_msg(message, conn) ::
    { :ok, conn } |
    { :ok, conn, :hibernate } |
    { :reply, message | [message], conn } |
    { :reply, message | [message], conn, :hibernate } |
    { :shutdown, conn }

  @doc """
  Handles all other messages received by an Erlang process.
  """
  defcallback handle_info(term, conn) ::
    { :ok, conn } |
    { :ok, conn, :hibernate } |
    { :reply, message | [message], conn } |
    { :reply, message | [message], conn, :hibernate } |
    { :shutdown, conn }

  @doc """
  Notified when the socket terminates.
  Useful for cleaning up, error notifications, etc.
  """
  defcallback terminate(terminate_reason, conn) :: :ok
end
