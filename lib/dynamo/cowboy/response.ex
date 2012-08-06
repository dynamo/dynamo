defmodule Dynamo.Cowboy.Response do
  require :cowboy_http_req, as: R

  @doc """
  Builds a new Dynamo.Cowboy.Response based on
  the original Cowboy request object.
  """
  def new(req) do
    { __MODULE__, req }
  end

  @doc """
  Returns the underlying cowboy request. This is used
  internally by Dynamo but may also be used by other
  developers (with caution).
  """
  def cowboy_request(res) do
    _(res)
  end

  @doc """
  Replies to the client with the given status, headers and body
  """
  def reply(status, headers, body, res) do
    { :ok, req } = :cowboy_http_req.reply(status, headers, body, _(res))
    _(req, res)
  end

  # Returns the original cowboy request object.

  defp _(req) do
    elem(req, 2)
  end

  defp _(value, req) do
    setelem(req, 2, value)
  end
end