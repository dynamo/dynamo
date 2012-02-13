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
    generate_match split(spec), []
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

  # Loops each segment checking finding dynamic matches.
  defp generate_match([h|t], acc) do
    final =
      case dynamic_match(h, []) do
      match: { :literal, literal }
        [literal|acc]
      match: { :identifier, _identifier, expr }
        [expr|acc]
      match: { :glob, _identifier, expr }
        if t != [], do:
          raise(InvalidSpec, message: "cannot have a *glob followed by other segments")

        [h_acc|t_acc] = acc
        [{ :|, 0, [h_acc, expr] } | t_acc]
      end

    generate_match(t, final)
  end

  defp generate_match([], acc) do
    List.reverse acc
  end

  # In a given segment, checks if there is a dynamic match.
  defp dynamic_match([?:|argument], []) do
    identifier = list_to_atom(argument)
    { :identifier, identifier, { identifier, 0, :quoted } }
  end

  defp dynamic_match([?*|argument], []) do
    identifier = list_to_atom(argument)
    { :glob, identifier, { identifier, 0, :quoted } }
  end

  defp dynamic_match([?:|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, 0, :quoted }
    expr = quote do
      unquote(List.reverse(buffer)) ++ unquote(var)
    end
    { :identifier, identifier, expr }
  end

  defp dynamic_match([?*|argument], buffer) do
    identifier = list_to_atom(argument)
    var = { identifier, 0, :quoted }
    expr = quote do
      [unquote(List.reverse(buffer)) ++ _ | _] = unquote(var)
    end
    { :glob, identifier, expr }
  end

  defp dynamic_match([h|t], buffer) do
    dynamic_match t, [h|buffer]
  end

  defp dynamic_match([], buffer) do
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
