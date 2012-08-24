defmodule Dynamo do
  def start do
    :application.start(:mimetypes)
    :application.start(:crypto)
    :application.start(:dynamo)
  end
end