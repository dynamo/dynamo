

defmodule Dynamo.Views.PathFinder do
  # TODO: Define handler behaviour

  def new(path) do
    { __MODULE__, File.expand_path(path) }
  end

  def find(template, { __MODULE__, path }) do
    query = File.join(path, template <> ".*")
    tmpl  = Enum.first File.wildcard(query)

    if tmpl do
      Dynamo.Views.Template[
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