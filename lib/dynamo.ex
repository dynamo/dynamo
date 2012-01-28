defmodule Dynamo do
  def run(name, options) do
    options = Orddict.merge default_options_for(name), options
    IO.puts "Running #{Orddict.get(options, :name)} on #{Orddict.get(options, :port)}"
    :misultin.start_link(options)
    if Orddict.get(options, :sleep, true), do: Erlang.timer.sleep(:infinity)
  end

  defp default_options_for(name) do
    [
      port: 3000,
      name: name,
      loop: fn(req) { name.service(req, {}) }
    ]
  end
end