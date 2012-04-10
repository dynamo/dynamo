defexception Dynamo.Router.InvalidSpec, message: "invalid route specification"

defmodule Dynamo.Router.Utils do
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
    generate_match binary_to_list(spec)
  end

  def generate_match(spec) do
    generate_match raw_split(spec), []
  end

  @doc """
  Generates a mounting representation that will match any
  route starting with the given `spec`.

  ## Examples

      generate_mount("/foo/:id") => ["foo", { :id, 0, nil } | _glob]

  """
  def generate_mount([h|_] = list) when is_binary(h) do
    [h|t] = List.reverse(list)
    glob  = { :glob, 0, nil }
    List.reverse [ { :|, 0, [h, glob] } | t ]
  end

  def generate_mount(spec) when is_binary(spec) do
    generate_mount binary_to_list(spec)
  end

  def generate_mount(spec) do
    generate_match raw_split(spec) ++ ['*glob'], []
  end

  @doc """
  Splits the given path into several segments.
  It ignores both leading and trailing slashes in the path.

  ## Examples

      split("/foo/bar") #=> ['foo', 'bar']

  """
  def split(bin) when is_binary(bin) do
    split binary_to_list(bin)
  end

  def split(t) do
    lc segment in raw_split(t), do: list_to_binary(segment)
  end

  ## Helpers

  # Loops each segment checking for matches.

  defp generate_match([h|t], acc) do
    handle_segment_match segment_match(h, []), t, acc
  end

  defp generate_match([], acc) do
    List.reverse(acc)
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
    match: [hs|ts]
      final = [{ :|, 0, [hs, expr] } | ts]
      List.reverse(final)
    else:
      expr
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

  # Helpers for splitting the path.

  defp raw_split([?/|t]) do
    raw_split t, [], []
  end

  defp raw_split(t) do
    raw_split t, [], []
  end

  defp raw_split(list, buffer, acc) when list == [] or list == [?/] do
    List.reverse [List.reverse(buffer)|acc]
  end

  defp raw_split([?/|t], buffer, acc) do
    raw_split t, [], [List.reverse(buffer)|acc]
  end

  defp raw_split([h|t], buffer, acc) do
    raw_split t, [h|buffer], acc
  end

  defp binary_from_buffer(buffer) do
    list_to_binary(List.reverse(buffer))
  end
end
