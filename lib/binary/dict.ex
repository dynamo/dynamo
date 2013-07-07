defmodule Binary.Dict do
  @moduledoc """
  This module implements a dictionary that forces the keys to
  be converted to binaries on insertion. Currently it is
  implemented using a `List.Dict` underneath, but this may
  change in the future.

  Check the `Dict` module for examples and documentation.
  """

  import Kernel, except: [to_binary: 1]
  @compile { :inline, to_binary: 1 }

  defp to_binary(key) do
    if is_binary(key), do: key, else: Binary.Chars.to_binary(key)
  end

  defmacrop dict(data) do
    quote do
      { Binary.Dict, unquote(data) }
    end
  end

  def new, do: dict([])

  def new(pairs) do
    dict Enum.map pairs, fn({ k, v }) -> { to_binary(k), v } end
  end

  def new(pairs, transform) when is_function(transform) do
    dict Enum.map pairs, fn(entry) ->
      { k, v } = transform.(entry)
      { to_binary(k), v }
    end
  end

  @doc false
  def keys(dict(data)) do
    lc { k, _ } inlist data, do: k
  end

  @doc false
  def values(dict(data)) do
    lc { _, v } inlist data, do: v
  end

  @doc false
  def size(dict(data)) do
    length(data)
  end

  @doc false
  def has_key?(dict(data), key) do
    :lists.keymember(to_binary(key), 1, data)
  end

  @doc false
  def get(dict(data), key, default // nil) do
    case :lists.keyfind(to_binary(key), 1, data) do
      { _, value } -> value
      false -> default
    end
  end

  @doc false
  def get!(dict(data), key) do
    case :lists.keyfind(to_binary(key), 1, data) do
      { _, value } -> value
      false -> raise(KeyError, key: key)
    end
  end

  @doc false
  def fetch(dict(data), key) do
    case :lists.keyfind(to_binary(key), 1, data) do
      { _, value } -> { :ok, value }
      false -> :error
    end
  end

  @doc false
  def put(dict(data), key, value) do
    key = to_binary(key)
    dict [{key, value}|keydelete(data, key)]
  end

  @doc false
  def put_new(dict, key, value) do
    update(dict, key, value, fn(v) -> v end)
  end

  @doc false
  def delete(dict(data), key) do
    dict keydelete(data, to_binary(key))
  end

  @doc false
  def merge(dict, enum, fun // fn(_k, _v1, v2) -> v2 end) do
    Enum.reduce enum, dict, fn({ k, v2 }, acc) ->
      k = to_binary(k)
      update(acc, k, v2, fn(v1) -> fun.(k, v1, v2) end)
    end
  end

  @doc false
  def update(dict(data), key, fun) do
    dict keyupdate(data, to_binary(key), fun)
  end

  @doc false
  def update(dict(data), key, initial, fun) do
    dict keyupdate(data, to_binary(key), initial, fun)
  end

  @doc false
  def empty(_) do
    dict([])
  end

  @doc false
  def to_list(dict(data)) do
    data
  end

  defp keydelete(data, key) do
    lc { k, _ } = tuple inlist data, key != k, do: tuple
  end

  defp keyupdate([{key, value}|dict], key, fun) do
    [{key, fun.(value)}|keydelete(dict, key)]
  end

  defp keyupdate([{_, _} = e|dict], key, fun) do
    [e|keyupdate(dict, key, fun)]
  end

  defp keyupdate([], key, _fun) do
    raise(KeyError, key: key)
  end

  defp keyupdate([{key, value}|dict], key, _initial, fun) do
    [{key, fun.(value)}|keydelete(dict, key)]
  end

  defp keyupdate([{_, _} = e|dict], key, initial, fun) do
    [e|keyupdate(dict, key, initial, fun)]
  end

  defp keyupdate([], key, initial, _fun) do
    [{key, initial}]
  end
end

defimpl Enumerable, for: Binary.Dict do
  def reduce({ Binary.Dict, data }, acc, fun), do: :lists.foldl(fun, acc, data)
  def count({ Binary.Dict, data }),            do: length(data)
  def member?({ Binary.Dict, data }, v),       do: :lists.member(v, data)
end

defimpl Access, for: Binary.Dict do
  def access({ Binary.Dict, data }, key) do
    case :lists.keyfind(to_binary(key), 1, data) do
      { _, value } -> value
      false -> nil
    end
  end
end

defimpl Inspect, for: Binary.Dict do
  import Inspect.Algebra

  def inspect({ Binary.Dict, data }, opts) do
    concat ["#Binary.Dict<", Inspect.List.inspect(data, opts), ">"]
  end
end
