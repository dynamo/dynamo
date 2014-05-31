defexception Dynamo.Router.InvalidSpecError, message: "invalid route specification"

defmodule Dynamo.Router.Utils do
  @moduledoc false

  @doc """
  Convert a given verb to its connection representation.
  """
  def normalize_verb(verb) do
    String.upcase(to_string(verb))
  end

  @doc """
  Generates a representation that will only match routes
  according to the given `spec`.

  ## Examples

      generate_match("/foo/:id") => ["foo", { :id, [], nil }]

  """
  def generate_match(spec, context \\ nil)

  def generate_match(spec, context) when is_binary(spec) do
    generate_match list_split(spec), context, [], []
  end

  def generate_match(match, _context) do
    { [], match }
  end

  @doc """
  Generates a fowarding representation that will match any
  route starting with the given `spec`.

  ## Examples

      generate_forward("/foo/:id") => ["foo", { :id, [], nil } | _glob]

  """
  def generate_forward(spec, context \\ nil)

  def generate_forward({ :_, _, _ }, context) do
    generate_forward "", context
  end

  def generate_forward(list, context) when is_list(list) do
    [h|t] = Enum.reverse(list)
    glob  = { :glob, [], context }
    { [], Enum.reverse [ { :|, [], [h, glob] } | t ] }
  end

  def generate_forward(spec, context) when is_binary(spec) do
    generate_match list_split(spec) ++ ['*glob'], context, [], []
  end

  @doc """
  Splits the given path into several segments.
  It ignores both leading and trailing slashes in the path.

  ## Examples

      split("/foo/bar") #=> ['foo', 'bar']

  """
  def split(bin) do
    for segment <- String.split(bin, "/"), segment != "", do: segment
  end

  ## Helpers

  # Loops each segment checking for matches.

  defp generate_match([h|t], context, vars, acc) do
    handle_segment_match segment_match(h, [], context), context, t, vars, acc
  end

  defp generate_match([], _context, vars, acc) do
    { vars |> Enum.uniq |> Enum.reverse, Enum.reverse(acc) }
  end

  # Handle each segment match. They can either be a
  # :literal ('foo'), an identifier (':bar') or a glob ('*path')

  def handle_segment_match({ :literal, literal }, context, t, vars, acc) do
    generate_match t, context, vars, [literal|acc]
  end

  def handle_segment_match({ :identifier, identifier, expr }, context, t, vars, acc) do
    generate_match t, context, [identifier|vars], [expr|acc]
  end

  def handle_segment_match({ :glob, identifier, expr }, context, t, vars, acc) do
    if t != [] do
      raise(Dynamo.Router.InvalidSpecError, message: "cannot have a *glob followed by other segments")
    end

    case acc do
      [hs|ts] ->
        acc = [{ :|, [], [hs, expr] } | ts]
        generate_match([], context, [identifier|vars], acc)
      _ ->
        { vars, expr } = generate_match([], context, [identifier|vars], [expr])
        { vars, hd(expr) }
    end
  end

  # In a given segment, checks if there is a match.

  defp segment_match([?:|argument], [], context) do
    identifier = list_to_atom(argument)
    { :identifier, identifier, { identifier, [], context } }
  end

  defp segment_match([?*|argument], [], context) do
    identifier = list_to_atom(argument)
    { :glob, identifier, { identifier, [], context } }
  end

  defp segment_match([?:|argument], buffer, context) do
    identifier = list_to_atom(argument)
    var = { identifier, [], context }
    expr = quote do
      unquote(binary_from_buffer(buffer)) <> unquote(var)
    end
    { :identifier, identifier, expr }
  end

  defp segment_match([?*|argument], buffer, context) do
    underscore = {:_, [], context}
    identifier = list_to_atom(argument)
    var = { identifier, [], context }
    expr = quote do
      [unquote(binary_from_buffer(buffer)) <> unquote(underscore) | unquote(underscore)] = unquote(var)
    end
    { :glob, identifier, expr }
  end

  defp segment_match([h|t], buffer, context) do
    segment_match t, [h|buffer], context
  end

  defp segment_match([], buffer, _context) do
    { :literal, binary_from_buffer(buffer) }
  end

  defp list_split(bin) do
    for segment <- String.split(bin, "/"), segment != "", do: List.from_char_data!(segment)
  end

  defp binary_from_buffer(buffer) do
    iodata_to_binary(Enum.reverse(buffer))
  end

  def is_function_exported?(module, function, arity) do
    case is_tuple(module) do
      true  ->
        function_exported?(elem(module, 0), function, arity + 1)
      false ->
        function_exported?(module, function, arity)
    end
  end
end
