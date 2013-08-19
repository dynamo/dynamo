defexception Dynamo.Router.InvalidSpecError, message: "invalid route specification"

defmodule Dynamo.Router.Utils do
  @moduledoc false

  @doc """
  Convert a given verb to its connection representation.
  """
  def normalize_verb(verb) do
    String.upcase(to_binary(verb))
  end

  @doc """
  Generates a representation that will only match routes
  according to the given `spec`.

  ## Examples

      generate_match("/foo/:id") => ["foo", { :id, [], nil }]

  """
  def generate_match(spec) when is_binary(spec) do
    generate_match list_split(spec), [], []
  end

  def generate_match(match) do
    { [], match }
  end

  @doc """
  Generates a fowarding representation that will match any
  route starting with the given `spec`.

  ## Examples

      generate_forward("/foo/:id") => ["foo", { :id, [], nil } | _glob]

  """
  def generate_forward({ :_, _, _ }) do
    generate_forward ""
  end

  def generate_forward(list) when is_list(list) do
    [h|t] = Enum.reverse(list)
    glob  = { :glob, [], nil }
    { [], Enum.reverse [ { :|, [], [h, glob] } | t ] }
  end

  def generate_forward(spec) when is_binary(spec) do
    generate_match list_split(spec) ++ ['*glob'], [], []
  end

  @doc """
  Splits the given path into several segments.
  It ignores both leading and trailing slashes in the path.

  ## Examples

      split("/foo/bar") #=> ['foo', 'bar']

  """
  def split(bin) do
    lc segment inlist String.split(bin, "/"), segment != "", do: segment
  end

  ## Helpers

  # Loops each segment checking for matches.

  defp generate_match([h|t], vars, acc) do
    handle_segment_match segment_match(h, []), t, vars, acc
  end

  defp generate_match([], vars, acc) do
    { vars |> Enum.uniq |> Enum.reverse, Enum.reverse(acc) }
  end

  # Handle each segment match. They can either be a
  # :literal ('foo'), an identifier (':bar') or a glob ('*path')

  def handle_segment_match({ :literal, literal }, t, vars, acc) do
    generate_match t, vars, [literal|acc]
  end

  def handle_segment_match({ :identifier, identifier, expr }, t, vars, acc) do
    generate_match t, [identifier|vars], [expr|acc]
  end

  def handle_segment_match({ :glob, identifier, expr }, t, vars, acc) do
    if t != [] do
      raise(Dynamo.Router.InvalidSpecError, message: "cannot have a *glob followed by other segments")
    end

    case acc do
      [hs|ts] ->
        acc = [{ :|, [], [hs, expr] } | ts]
        generate_match([], [identifier|vars], acc)
      _ ->
        { vars, expr } = generate_match([], [identifier|vars], [expr])
        { vars, hd(expr) }
    end
  end

  # In a given segment, checks if there is a match.

  defp segment_match([?:|argument], []) do
    identifier = list_to_atom(argument)
    { :identifier, identifier, { identifier, [], nil } }
  end

  defp segment_match([?*|argument], []) do
    identifier = list_to_atom(argument)
    { :glob, identifier, { identifier, [], nil } }
  end

  defp segment_match([?:|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, [], nil }
    expr = quote do
      unquote(binary_from_buffer(buffer)) <> unquote(var)
    end
    { :identifier, identifier, expr }
  end

  defp segment_match([?*|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, [], nil }
    expr = quote [hygiene: [vars: false]] do
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
    lc segment inlist String.split(bin, "/"), segment != "", do: String.to_char_list!(segment)
  end

  defp binary_from_buffer(buffer) do
    iolist_to_binary(Enum.reverse(buffer))
  end
end
