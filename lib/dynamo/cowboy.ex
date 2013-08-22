defmodule Dynamo.Cowboy do
  @moduledoc """
  Provides the interface to Cowboy webserver.
  Check `run/2` for more information.
  """

  @doc """
  Runs the given module with the given options:

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
  def run(main, options // []) do
    :application.start(:ranch)
    :application.start(:cowboy)

    env  = options[:env]
    ssl  = options[:ssl]
    host = options[:host] || "localhost"

    options = Enum.reduce [:env, :ssl, :host], options, Keyword.delete(&2, &1)

    case host do
      _ in ["localhost", nil] ->
        :ok
      host when is_binary(host) ->
        ip = host |> String.split(".") |> Enum.map(binary_to_integer(&1)) |> list_to_tuple
        options = Keyword.put(options, :ip, ip)
    end

    if ssl do
      :application.start(:crypto)
      :application.start(:public_key)
      :application.start(:ssl)
      https = https_options(main, Keyword.merge(options, ssl))
      log(main, :https, env, host, https)
      start_listener(:https, main, https)
    end

    http = http_options(main, options)
    log(main, :http, env, host, http)
    start_listener(:http, main, http)
  end

  def shutdown(main) do
    :cowboy.stop_listener(main.HTTP)
    :cowboy.stop_listener(main.HTTPS)
    :ok
  end

  ## Helpers

  @http_options  [port: 4000]
  @https_options [port: 4040]

  defp start_listener(kind, main, options) do
    acceptors = options[:acceptors] || 100
    dispatch  = :cowboy_router.compile(options[:dispatch]  || dispatch_for(main))
    options   = Enum.reduce [:acceptors, :dispatch], options, Keyword.delete(&2, &1)

    ref = Module.concat(main, kind |> to_string |> String.upcase)
    apply(:cowboy, :"start_#{kind}", [ref, acceptors, options, [env: [dispatch: dispatch]]])
  end

  defp http_options(_main, options) do
    Keyword.merge @http_options, options
  end

  defp https_options(main, options) do
    options = Keyword.merge @https_options, options
    options = Enum.reduce [:keyfile, :certfile, :cacertfile], options, normalize_ssl_file(main, &2, &1)
    options = Enum.reduce [:password], options, to_char_list(&2, &1)
    options
  end

  defp log(main, kind, env, host, options) do
    unless options[:verbose] == false do
      IO.puts "Running #{inspect main} at #{kind}://#{host}:#{options[:port]} with Cowboy on #{env}"
    end
  end

  defp dispatch_for(main) do
    [{ :_, [ {:_, Dynamo.Cowboy.Handler, main } ] }]
  end

  defp normalize_ssl_file(main, options, key) do
    value = options[key]

    if nil?(value) do
      options
    else
      new = Path.expand(value, main.root) |> to_char_list
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
