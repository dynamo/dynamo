defmodule Dynamo.HTTP do
  @moduledoc """
  This module is a namespace to many conveniences
  that allow developers to manipulate cookies and
  session, redirect, render templates and others.

  By using this module, all those dependencies are
  imported into the current module. It is guaranteed
  that using this module will not define any function
  in the target module, which makes it convenient to
  define helper functions and others.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      import Dynamo.HTTP.Cookies
      import Dynamo.HTTP.Halt
      import Dynamo.HTTP.Hibernate
      import Dynamo.HTTP.Redirect
      import Dynamo.HTTP.Render
      import Dynamo.HTTP.Session
    end
  end
end
