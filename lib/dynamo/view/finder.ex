defmodule Dynamo.View.Finder do
  use Behaviour

  @doc """
  Initializes the finder with the relevant
  information.
  """
  defcallback new(info)

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
  @behaviour Dynamo.View.Finder

  def new(path) do
    { __MODULE__, File.expand_path(path) }
  end

  def all({ __MODULE__, _path }) do
    nil
  end

  def find(template, { __MODULE__, path }) do
    query = File.join(path, template <> ".*")
    tmpl  = Enum.first File.wildcard(query)

    if tmpl do
      Dynamo.View.Template[
        key: template,
        updated_at: File.stat!(tmpl).mtime,
        identifier: tmpl,
        handler: extname(tmpl),
        format: extname(File.rootname(tmpl)),
        source: File.read!(tmpl)
      ]
    end
  end

  defp extname(path) do
    case File.extname(path) do
      "." <> ext -> ext
      ext -> ext
    end
  end
end