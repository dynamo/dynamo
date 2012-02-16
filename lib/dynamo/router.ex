defexception Dynamo::Router::InvalidSpec, message: "invalid route specification"

defmodule Dynamo::Router do
  # Generates a representation that will only match routes according to the
  # given `spec`.
  #
  # ## Examples
  #
  #     generate_match("/foo/:id") => ['foo', { :id, 0, :quoted }]
  #
  def generate_match(spec) do
    generate_match split(to_char_list(spec)), []
  end

  # Splits the given path into several segments.
  # It ignores both leading and trailing slashes in the path.
  #
  # ## Examples
  #
  #     split("/foo/bar") #=> ['foo', 'bar']
  #
  def split([?/|t]) do
    split t
  end

  def split(t) do
    split t, [], []
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
      raise(InvalidSpec, message: "cannot have a *glob followed by other segments")
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
    { :identifier, identifier, { identifier, 0, :quoted } }
  end

  defp segment_match([?*|argument], []) do
    identifier = list_to_atom(argument)
    { :glob, identifier, { identifier, 0, :quoted } }
  end

  defp segment_match([?:|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, 0, :quoted }
    expr = quote do
      unquote(List.reverse(buffer)) ++ unquote(var)
    end
    { :identifier, identifier, expr }
  end

  defp segment_match([?*|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, 0, :quoted }
    expr = quote do
      [unquote(List.reverse(buffer)) ++ _ | _] = unquote(var)
    end
    { :glob, identifier, expr }
  end

  defp segment_match([h|t], buffer) do
    segment_match t, [h|buffer]
  end

  defp segment_match([], buffer) do
    { :literal, List.reverse buffer }
  end

  # Helpers for splitting the path.
  defp split(list, buffer, acc) when list == [] orelse list == [?/] do
    List.reverse [List.reverse(buffer)|acc]
  end

  defp split([?/|t], buffer, acc) do
    split t, [], [List.reverse(buffer)|acc]
  end

  defp split([h|t], buffer, acc) do
    split t, [h|buffer], acc
  end
end
