defmodule Dynamo do
  def run(name, options) do
    options = Orddict.merge default_options_for(name), options
    IO.puts "Running #{Orddict.fetch(options, :name, nil)} on #{Orddict.fetch(options, :port, nil)}"
    :misultin.start_link(options)
  end

  defp default_options_for(name) do
    [
      port: 3000,
      name: name,
      loop: fn(req) { name.service(req, {}) }
    ]
  end
end