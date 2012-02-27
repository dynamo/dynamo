Code.append_path "deps/ibrowse/ebin"
{ :ok, _ } = Erlang.ibrowse.start

defmodule HTTPClient do
  def new(path) do
    { __MODULE__, to_char_list(path) }
  end

  def request(verb, path, headers // [], client) do
    fullpath = elem(client, 2) ++ to_char_list(path)
    { :ok, status, headers, body } = Erlang.ibrowse.send_req(fullpath, decode_headers(headers), verb)
    { list_to_integer(status), encode_headers(headers), to_binary(body) }
  end

  defp decode_headers(headers) do
    Enum.map headers, fn({k,v}) -> { to_char_list(k), to_char_list(v) } end
  end

  defp encode_headers(headers) do
    Enum.map headers, fn({k,v}) -> { to_binary(k), to_binary(v) } end
  end
end