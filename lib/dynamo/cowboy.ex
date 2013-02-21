defmodule Dynamo.Cowboy do
  @moduledoc """
  Provides the interface to Cowboy webserver.
  Check `run/2` for more information.
  """

  @doc """
  Runs the given app with the given options:

  * `port` - the port to run the server (defaults to 4000)

  * `acceptors` - the number of acceptors for the listener

  * `max_connections` - max number of connections supported

  * `ssl` - SSL options for the server. It accepts all options
    mentioned above plus the configuration options accepted
    by the [`ssl` erlang module](http://www.erlang.org/doc/man/ssl.html)
    (like keyfile, certfile, cacertfile and others).

  ## Example

      Dynamo.Cowboy.run MyApp, port: 80

  """
  def run(app, options // []) do
    :application.start(:ranch)
    :application.start(:cowboy)

    env     = options[:env]
    ssl     = options[:ssl]
    options = Enum.reduce [:env, :ssl], options, Keyword.delete(&2, &1)

    if ssl do
      :application.start(:crypto)
      :application.start(:public_key)
      :application.start(:ssl)
      https = https_options(app, Keyword.merge(options, ssl))
      log(app, :https, env, https)
      start_listener(:https, app, https)
    end

    http = http_options(app, options)
    log(app, :http, env, http)
    start_listener(:http, app, http)
  end

  def shutdown(app) do
    :cowboy.stop_listener(app.HTTP)
    :cowboy.stop_listener(app.HTTPS)
    :ok
  end

  ## Helpers

  @http_options  [port: 4000]
  @https_options [port: 4040]

  defp start_listener(kind, app, options) do
    acceptors = options[:acceptors] || 100
    dispatch  = options[:dispatch]  || dispatch_for(app)
    options   = Enum.reduce [:acceptors, :dispatch], options, Keyword.delete(&2, &1)

    ref = Module.concat(app, kind |> to_binary |> String.upcase)
    apply(:cowboy, :"start_#{kind}", [ref, acceptors, options, [dispatch: dispatch]])
  end

  defp http_options(_app, options) do
    Keyword.merge @http_options, options
  end

  defp https_options(app, options) do
    options = Keyword.merge @https_options, options
    Enum.reduce [:keyfile, :certfile, :cacertfile], options, normalize_ssl_file(app, &2, &1)
    Enum.reduce [:password], options, to_char_list(&2, &1)
  end

  defp log(app, kind, env, options) do
    unless options[:verbose] == false do
      IO.puts "Running #{inspect app} at #{kind}://localhost:#{options[:port]} with Cowboy on #{env}"
    end
  end

  defp dispatch_for(app) do
    [{ :_, [ {:_, Dynamo.Cowboy.Handler, app } ] }]
  end

  defp normalize_ssl_file(app, options, key) do
    value = options[key]

    if nil?(value) do
      options
    else
      new = Path.expand(value, app.root) |> to_char_list
      Keyword.put(options, key, new)
    end
  end

  defp to_char_list(options, key) do
    if value = options[key] do
      Keyword.put options, key, to_char_list(value)
    else
      options
    end
  end
end
