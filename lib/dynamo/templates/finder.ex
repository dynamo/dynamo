defprotocol Dynamo.Templates.Finder do
  @moduledoc """
  Defines the protocol required for finding templates.
  """

  @doc """
  Returns true if templates require precompilation.
  """
  @spec requires_precompilation?(t) :: boolean
  def requires_precompilation?(finder)

  @doc """
  Attempts to find a template given by name in the
  current finder.

  Returns a `Dynamo.Template` or nil in case a template
  can't be found.
  """
  @spec find(t, query :: binary) :: Dynamo.Template.t
  def find(finder, query)

  @doc """
  Returns all templates available in this finder.
  This is used for precompilation of templates.
  Must return nil if this finder already holds
  precompiled templates (i.e. `requires_precompilation?` is true).
  """
  @spec all(t) :: [Dynamo.Template.t] | nil
  def all(finder)

  @doc """
  Returns the given template source.
  """
  @spec source(t, Dynamo.Template.t) :: binary
  def source(finder, template)
end

defimpl Dynamo.Templates.Finder, for: BitString do
  def requires_precompilation?(string) do
    not File.exists?(string)
  end

  def all(root) do
    lc path inlist Path.wildcard("#{root}/**/*.*") do
      key = Path.relative_to path, root
      build(root, Path.rootname(key), path)
    end
  end

  def find(root, key) do
    query = Path.join(root, escape(key) <> ".*")
    path  = Enum.first Path.wildcard(query)
    if path, do: build(root, key, path)
  end

  def source(_root, Dynamo.Template[identifier: path]) do
    File.read!(path)
  end

  defp escape(key) do
    bc <<code>> inbits key do
      << if code in [?[, ?], ?{, ?}, ?*, ??] do
           << ?\\, code >>
         else
           << code >>
         end :: binary >>
    end
  end

  defp build(root, key, path) do
    Dynamo.Template[
      key: key,
      updated_at: File.stat!(path).mtime,
      identifier: path,
      handler: Dynamo.Templates.Handler.get!(extname(path)),
      format: extname(Path.rootname(path)),
      finder: root
    ]
  end

  defp extname(path) do
    case Path.extname(path) do
      "." <> ext -> ext
      ""  -> nil
      ext -> ext
    end
  end
end

defimpl Dynamo.Templates.Finder, for: Atom do
  def all(atom),                      do: atom.all
  def find(atom, key),                do: atom.find(key)
  def source(atom, key),              do: atom.source(key)
  def requires_precompilation?(atom), do: atom.requires_precompilation?
end
