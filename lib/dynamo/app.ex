defmodule Dynamo.App do
  @moduledoc """
  `Dynamo.App` is a module that helps you define your
  application behavior globally.

  A `Dynamo.App` can be used on top of a `Dynamo.Router`,
  so you can route and point to other endpoints easily.

  ## Not found

  Each `Dynamo.Router` has a `not_found` hook that is
  invoked whenever a route does not match. However, if
  a developer wants to consistently customize how a 404
  page looks like, he shouldn't need to customize each
  `Dynamo.Router` in his application. That's when
  `Dynamo.App` comes in.

  By default, a `Dynamo.Router` sets the response status
  to 404 whenever a route doesn't match. `Dynamo.App` then
  intercepts all not sent responses with status 404 and
  invokes its own `not_found` function, which can then be
  customized by the developer, for example:

      def not_found(conn) do
        html conn, "404.eex"
      end

  """

  @doc false
  defmacro __using__(_) do
    quote do
      @dynamo_app true

      @doc false
      def dynamo_app? do
        true
      end

      use Dynamo.Support.Once

      use_once Dynamo.App.Config
      use_once Dynamo.App.NotFound
    end
  end
end