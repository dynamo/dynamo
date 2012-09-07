defmodule Dynamo.View.Finder do
  @moduledoc """
  This module defines the basic behavior
  required by Elixir for finding templates
  in the filesystem
  """

  use Behaviour

  @doc """
  Initializes the finder with the relevant
  information.
  """
  defcallback new(info)

  @doc """
  Returns true it provides compiled templates.
  """
  defcallback compilable?(self)

  @doc """
  Returns all templates for this finder.

  This is used for eager template
  compilation in production. In case
  eager compilation is not supported,
  it should simply return nil.
  """
  defcallback all(self)

  @doc """
  Attempts to find a template given by
  `query` in the current finder `self`.

  Returns a `Dynamo.View.Template` or
  nil in case a template can't be found.
  """
  defcallback find(query, self)
end

defmodule Dynamo.View.PathFinder do
  @moduledoc false
  @behaviour Dynamo.View.Finder

  def new(root) do
    { __MODULE__, File.expand_path(root) }
  end

  def compilable?(_) do
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

  defp build(key, path) do
    Dynamo.View.Template[
      key: key,
      updated_at: File.stat!(path).mtime,
      identifier: path,
      handler: extname(path),
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