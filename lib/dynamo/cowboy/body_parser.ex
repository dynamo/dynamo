defmodule Dynamo.Cowboy.BodyParser do
  @moduledoc false

  require :cowboy_req, as: R

  def parse(dict, req) do
    { :ok, type, req } = R.parse_header("content-type", req)
    parse_body(type, dict, req)
  end

  defp parse_body({ "application", "x-www-form-urlencoded", _ }, dict, req) do
    { :ok, body, req } = R.body(req)
    { Dynamo.Connection.QueryParser.parse(body, dict), req }
  end

  defp parse_body({ "multipart", style, _ }, dict, req) when style in ["form-data", "mixed"] do
    { tuples, req } = parse_multipart(R.multipart_data(req), nil, [])
    dict = Enum.reduce(tuples, dict, &Dynamo.Connection.QueryParser.reduce(&1, &2))
    { dict, req }
  end

  defp parse_body(_, dict, req) do
    { dict, req }
  end

  defp parse_multipart({ :eof, req }, _tmp_dir, acc) do
    { acc, req }
  end

  defp parse_multipart({ :headers, headers, req }, tmp_dir, acc) do
    case parse_multipart_headers(headers) do
      { name, nil } ->
        { body, req } = parse_multipart_body(R.multipart_data(req), "")
        parse_multipart(R.multipart_data(req), tmp_dir, [{ name, body }|acc])

      { name, Dynamo.Connection.File[] = file } ->
        tmp_dir = get_tmp_dir(tmp_dir)

        { path, { :ok, req } } = Dynamo.Connection.Utils.random_file("uploaded",
          tmp_dir, &parse_multipart_file(R.multipart_data(req), &1))

        parse_multipart(R.multipart_data(req), tmp_dir, [{ name, file.path(path) }|acc])

      nil ->
        { :ok, req } = R.multipart_skip(req)
        parse_multipart(R.multipart_data(req), tmp_dir, acc)
    end
  end

  defp parse_multipart_body({ :body, tail, req }, body) do
    parse_multipart_body(R.multipart_data(req), body <> tail)
  end

  defp parse_multipart_body({ :end_of_part, req }, body) do
    { body, req }
  end

  defp parse_multipart_file({ :body, body, req }, file) do
    :file.write(file, body)
    parse_multipart_file(R.multipart_data(req), file)
  end

  defp parse_multipart_file({ :end_of_part, req }, _file) do
    { :ok, req }
  end

  defp parse_multipart_headers(headers) do
    case List.keyfind(headers, "content-disposition", 0) do
      { _, disposition } ->
        [_|parts] = String.split(disposition, ";")
        parts     = lc part inlist parts, do: split_equals(part)

        case List.keyfind(parts, "name", 0) do
          { _, name } ->
            case List.keyfind(parts, "filename", 0) do
              { _, filename } ->
                { _, type } = List.keyfind(headers, "content-type", 0) || { "content-type", nil }
                { name, Dynamo.Connection.File[name: name, filename: filename, content_type: type, path: nil] }
              _ ->
                { name, nil }
            end
          _ -> nil
        end
      _ -> nil
    end
  end

  defp get_tmp_dir(nil),   do: Dynamo.Connection.Utils.tmp_dir
  defp get_tmp_dir(other), do: other

  defp split_equals(binary) do
    case String.split(binary, "=", global: false) do
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
