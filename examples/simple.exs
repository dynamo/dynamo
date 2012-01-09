# Run this app with:
#
#   elixir -pa ebin -pa deps/misultin/ebin examples/simple.exs 

defmodule MyApp do
  use Dynamo::App
  
  get "/foo/bar" do
    
  end
end

MyApp.run