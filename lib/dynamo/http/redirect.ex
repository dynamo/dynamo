defmodule Dynamo.HTTP.Redirect do
  @moduledoc """
  Conveniences to redirect a connection.
  To use them, just import this module.
  """

  @doc """
  Redirects the connection to the given endpoint.

  ## Options

  * `to` - the path or url to redirect to
  * `status` - the status to redirect to, defaults to 302
  * `format` - the response format

  ## Examples

      redirect conn, to: "/"
      redirect conn, to: "/", format: :html

  """
  def redirect(conn, opts) do
    status = Keyword.get(opts, :status, 302)

    if to = opts[:to] do
      # The scheme name consist of a letter followed by any
      # combination of letters, digits, and the plus ("+"),
      # period ("."), or hyphen ("-") characters; and is
      # terminated by a colon (":").
      unless to =~ %r{^(\w[\w+.-]*:|//).*} do
        to = conn.host_url <> to
      end
    else
      raise ArgumentError, message: "Expected :to to be given to redirect"
    end

    if format = opts[:format] do
      mime = :mimetypes.ext_to_mimes(to_string(format))
      conn = conn.resp_content_type(hd(mime))
    end

    conn
      .put_resp_header("location", to)
      .resp(status, redirect_body(conn.resp_content_type, to))
  end

  @doc """
  A convenience that redirects and halts straight away.
  """
  def redirect!(conn, opts) do
    Dynamo.HTTP.Halt.halt! redirect(conn, opts)
  end

  defp redirect_body("text/html", to), do: %s[<html><body>You are being <a href="#{to}">redirected</a>.</body></html>]
  defp redirect_body(_, _), do: ""
end