defmodule Dynamo do
  def start do
    :application.start(:mimetypes)
    :application.start(:dynamo)
  end
end