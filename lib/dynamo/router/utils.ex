defexception Dynamo.Router.InvalidSpec, message: "invalid route specification"

defmodule Dynamo.Router.Utils do
  @moduledoc false

  @doc """
  Generates a representation that will only match routes
  according to the given `spec`.

  ## Examples

      generate_match("/foo/:id") => ["foo", { :id, 0, nil }]

  """
  def generate_match([h|_] = match) when is_binary(h) do
    match
  end

  def generate_match(spec) when is_binary(spec) do
    generate_match list_split(spec), []
  end

  @doc """
  Generates a fowarding representation that will match any
  route starting with the given `spec`.

  ## Examples

      generate_forward("/foo/:id") => ["foo", { :id, 0, nil } | _glob]

  """
  def generate_forward([h|_] = list) when is_binary(h) do
    [h|t] = Enum.reverse(list)
    glob  = { :glob, 0, nil }
    Enum.reverse [ { :|, 0, [h, glob] } | t ]
  end

  def generate_forward(spec) when is_binary(spec) do
    generate_match list_split(spec) ++ ['*glob'], []
  end

  @doc """
  Splits the given path into several segments.
  It ignores both leading and trailing slashes in the path.

  ## Examples

      split("/foo/bar") #=> ['foo', 'bar']

  """
  def split(bin) do
    lc segment inlist String.split(bin, "/", global: true), segment != "", do: segment
  end

  ## Helpers

  # Loops each segment checking for matches.

  defp generate_match([h|t], acc) do
    handle_segment_match segment_match(h, []), t, acc
  end

  defp generate_match([], acc) do
    Enum.reverse(acc)
  end

  # Handle each segment match. They can either be a
  # :literal ('foo'), an identifier (':bar') or a glob ('*path')

  def handle_segment_match({ :literal, literal }, t, acc) do
    generate_match t, [literal|acc]
  end

  def handle_segment_match({ :identifier, _identifier, expr }, t, acc) do
    generate_match t, [expr|acc]
  end

  def handle_segment_match({ :glob, _identifier, expr }, t, acc) do
    if t != [] do
      raise(Dynamo.Router.InvalidSpec, message: "cannot have a *glob followed by other segments")
    end

    case acc do
      [hs|ts] ->
        final = [{ :|, 0, [hs, expr] } | ts]
        Enum.reverse(final)
      _ -> expr
    end
  end

  # In a given segment, checks if there is a match.

  defp segment_match([?:|argument], []) do
    identifier = list_to_atom(argument)
    { :identifier, identifier, { identifier, 0, nil } }
  end

  defp segment_match([?*|argument], []) do
    identifier = list_to_atom(argument)
    { :glob, identifier, { identifier, 0, nil } }
  end

  defp segment_match([?:|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, 0, nil }
    expr = quote do
      unquote(binary_from_buffer(buffer)) <> unquote(var)
    end
    { :identifier, identifier, expr }
  end

  defp segment_match([?*|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, 0, nil }
    expr = quote hygiene: false do
      [unquote(binary_from_buffer(buffer)) <> _ | _] = unquote(var)
    end
    { :glob, identifier, expr }
  end

  defp segment_match([h|t], buffer) do
    segment_match t, [h|buffer]
  end

  defp segment_match([], buffer) do
    { :literal, binary_from_buffer(buffer) }
  end

  defp list_split(bin) do
    lc segment inlist String.split(bin, "/", global: true), segment != "", do: binary_to_list(segment)
  end

  defp binary_from_buffer(buffer) do
    list_to_binary(Enum.reverse(buffer))
  end
end
