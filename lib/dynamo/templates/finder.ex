defmodule Dynamo.Templates.Finder do
  @moduledoc """
  This module defines the basic behavior
  required by Elixir for finding templates
  in the filesystem
  """

  use Behaviour

  @type finder :: tuple

  @doc """
  Initializes the finder with the relevant
  information.
  """
  defcallback new(info :: term) :: finder

  @doc """
  Returns true if templates can be eager
  compiled. If so, the behaviour also needs
  to implement a `all(self)` function that
  returns all templates hold by the finder.
  """
  defcallback eager?(finder) :: boolean

  @doc """
  Attempts to find a template given by
  `query` in the current finder.

  Returns a `Dynamo.Template` or
  nil in case a template can't be found.
  """
  defcallback find(query :: binary, finder) :: Dynamo.Template.t

  @doc """
  Returns a filesystem path to be watched
  if this finder maps to somewhere in the
  filesystem. Returns nil otherwise.
  """
  defcallback to_path(finder) :: binary | nil
end

defmodule Dynamo.Templates.PathFinder do
  @moduledoc false
  @behaviour Dynamo.Templates.Finder

  def new(root) do
    { __MODULE__, File.expand_path(root) }
  end

  def eager?(_) do
    true
  end

  def all({ __MODULE__, root }) do
    lc path inlist File.wildcard("#{root}/**/*.*") do
      key = :binary.replace(path, root <> "/", "")
      build(File.rootname(key), path)
    end
  end

  def find(key, { __MODULE__, root }) do
    query = File.join(root, key <> ".*")
    path  = Enum.first File.wildcard(query)
    if path, do: build(key, path)
  end

  def to_path({ __MODULE__, path }) do
    path
  end

  defp build(key, path) do
    Dynamo.Template[
      key: key,
      updated_at: File.stat!(path).mtime,
      identifier: path,
      handler: Dynamo.Templates.Handler.get!(extname(path)),
      format: extname(File.rootname(path)),
      source: File.read!(path)
    ]
  end

  defp extname(path) do
    case File.extname(path) do
      "." <> ext -> ext
      ""  -> nil
      ext -> ext
    end
  end
end