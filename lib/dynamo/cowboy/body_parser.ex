defmodule Dynamo.Cowboy.BodyParser do
  require :cowboy_req, as: R

  @moduledoc false

  def parse(dict, req) do
    { :ok, type, req } = R.parse_header("content-type", req)
    parse_body(type, dict, req)
  end

  defp parse_body({ "application", "x-www-form-urlencoded", _ }, dict, req) do
    { :ok, body, req } = R.body(req)
    { Dynamo.HTTP.QueryParser.parse(body, dict), req }
  end

  defp parse_body({ "multipart", style, _ }, dict, req) when style in ["form-data", "mixed"] do
    { tuples, req } = parse_multipart(R.multipart_data(req), nil, nil, [])
    dict = Enum.reduce(tuples, dict, Dynamo.HTTP.QueryParser.reduce(&1, &2))
    { dict, req }
  end

  defp parse_body(_, dict, req) do
    { dict, req }
  end

  defp parse_multipart({ :eof, req }, nil, nil, acc) do
    { acc, req }
  end

  defp parse_multipart({ :headers, headers, req }, nil, nil, acc) do
    parse_multipart(R.multipart_data(req), headers, "", acc)
  end

  defp parse_multipart({ :body, tail, req }, headers, body, acc) do
    parse_multipart(R.multipart_data(req), headers, body <> tail, acc)
  end

  defp parse_multipart({ :end_of_part, req }, headers, body, acc) do
    acc = multipart_entry(headers, body, acc)
    parse_multipart(R.multipart_data(req), nil, nil, acc)
  end

  defp multipart_entry(headers, body, acc) do
    case List.keyfind(headers, "content-disposition", 0) do
      { _, value } ->
        [_|parts] = String.split(value, ";", global: true)
        parts     = lc part inlist parts, do: to_multipart_kv(part)

        case List.keyfind(parts, "name", 0) do
          { "name", name } ->
            entry =
              case List.keyfind(parts, "filename", 0) do
                { "filename", filename } ->
                  { _, type } = List.keyfind(headers, "content-type", 0) || { "content-type", nil }
                  { name, Dynamo.HTTP.File.new(name: name, filename: filename, content_type: type, body: body) }
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
    case String.split(binary, "=") do
      [h]   -> { trim(h), nil }
      [h,t] -> { trim(h), strip_quotes(t) }
    end
  end

  defp strip_quotes(<<?", remaining :: binary>>) do
    binary_part(remaining, 0, size(remaining) - 1)
  end

  defp strip_quotes(other) do
    other
  end

  defp trim(<<?\s, rest :: binary>>),   do: trim(rest)
  defp trim(rest) when is_binary(rest), do: rest
end