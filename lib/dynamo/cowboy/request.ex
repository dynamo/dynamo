defmodule Dynamo.Cowboy.Request do
  require :cowboy_http_req, as: R

  @doc """
  Builds a new Dynamo.Cowboy.Request based on
  the original Cowboy request object.
  """
  def new(req) do
    { segments, req } = R.path(req)
    { type, req }     = R.parse_header(:"Content-Type", req)
    { params, req }   = parse_body(type, req)
    { __MODULE__, req, segments, [], params }
  end

  @doc """
  Returns the query string as a binary.
  """
  def query_string(req) do
    { query_string, _ } = R.raw_qs _(req)
    query_string
  end

  @doc """
  Returns a function that allows a developer to retrieve params
  either from query string or from post body.
  """
  def params(req) do
    fn(name) ->
      name = to_binary(name)
      case R.qs_val(name, _(req), nil) do
        { nil, _ } ->
          case List.keyfind(elem(req, 5), name, 1) do
            { ^name, value } -> value
            nil -> nil
          end
        { value, _ } -> value
      end
    end
  end

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  def path_info_segments(req) do
    elem(req, 3)
  end

  @doc """
  Returns the request path relative to the forwarding endpoint
  as a binary.
  """
  def path_info(req) do
    to_path path_info_segments(req)
  end

  @doc """
  Returns the full path segments, as received by the web server.
  """
  def path_segments(req) do
    { segments, _ } = R.path _(req)
    segments
  end

  @doc """
  Returns the full path as a binary, as received by the web server.
  """
  def path(req) do
    { binary, _ } = R.raw_path _(req)
    binary
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as segments.
  """
  def script_info_segments(req) do
    elem(req, 4)
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as binary.
  """
  def script_info(req) do
    to_path script_info_segments(req)
  end

  @doc """
  Returns the HTTP method as an atom.

  ## Examples

      request.method #=> :GET

  """
  def method(req) do
    { verb, _ } = R.method _(req)
    verb
  end

  @doc """
  Returns the HTTP version.
  """
  def version(req) do
    { version, _ } = R.version _(req)
    version
  end

  ## Dynamo API

  # Mounts the request by setting the new path information to the
  # *segments*. Both script_info/1 and path_segments/1 are updated.
  # The segments given must be a suffix of the current path segments.
  @doc false
  def forward_to(req, segments, _target) do
    current = path_info_segments(req)
    { prefix, ^segments } = Enum.split current, length(current) - length(segments)
    req = setelem(req, 3, segments)
    req = setelem(req, 4, script_info_segments(req) ++ prefix)
    req
  end

  ## Helpers

  defp to_path(segments) do
    "/" <> Enum.join(segments, "/")
  end

  defp parse_body({ "application", "x-www-form-urlencoded", _ }, req) do
    R.body_qs(req)
  end

  defp parse_body({ "multipart", style, _ }, req) when style in ["form-data", "mixed"] do
    parse_multipart(R.multipart_data(req), nil, nil, [])
  end

  defp parse_body(_, req) do
    { [], req }
  end

  defp parse_multipart({ :eof, req }, nil, nil, acc) do
    { List.reverse(acc), req }
  end

  defp parse_multipart({ { :headers, headers }, req }, nil, nil, acc) do
    parse_multipart(R.multipart_data(req), headers, "", acc)
  end

  defp parse_multipart({ { :body, tail }, req }, headers, body, acc) do
    parse_multipart(R.multipart_data(req), headers, body <> tail, acc)
  end

  defp parse_multipart({ :end_of_part, req }, headers, body, acc) do
    acc = multipart_entry(headers, body, acc)
    parse_multipart(R.multipart_data(req), nil, nil, acc)
  end

  defp multipart_entry(headers, body, acc) do
    case List.keyfind(headers, "Content-Disposition", 1) do
      { _, value } ->
        [_|parts] = Binary.split(value, ";", global: true)
        parts     = lc part inlist parts, do: to_multipart_kv(part)

        case List.keyfind(parts, "name", 1) do
          { "name", name } ->
            entry =
              case List.keyfind(parts, "filename", 1) do
                { "filename", filename } ->
                  { _, type } = List.keyfind(headers, :"Content-Type", 1) || { :"Content-Type", nil }
                  { name, Dynamo.Request.File.new(name: name, filename: filename, content_type: type, body: body) }
                _ ->
                  { name, body }
              end

            [entry|acc]
          _ -> acc
        end
      _ -> acc
    end
  end

  defp to_multipart_kv(binary) do
    case Binary.split(binary, "=") do
      [h]     -> { trim(h), nil }
      [h,t|_] -> { trim(h), strip_quotes(t) }
    end
  end

  defp strip_quotes(<<?", remaining | :binary>>) do
    binary_part(remaining, 0, size(remaining) - 1)
  end

  defp strip_quotes(other) do
    other
  end

  defp trim(<<?\s, rest | :binary>>),   do: trim(rest)
  defp trim(rest) when is_binary(rest), do: rest

  defp _(req) do
    elem(req, 2)
  end
end