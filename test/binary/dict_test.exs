defmodule Binary.DictTest do
  use ExUnit.Case, async: true

  test :new_with_pairs do
    dict = Binary.Dict.new [foo: "bar"]
    assert Binary.Dict.to_list(dict) == [{ "foo", "bar" }]
  end

  test :new_with_pairs_and_transformation do
    dict = Binary.Dict.new [foo: "bar"], fn({ k, v }) -> { k, atom_to_binary(k) <> v } end
    assert Binary.Dict.to_list(dict) == [{ "foo", "foobar" }]
  end

  test :keys do
    assert Binary.Dict.keys(sample_dict) == ["foo", "baz"]
  end

  test :values do
    assert Binary.Dict.values(sample_dict) == ["bar", "bat"]
  end

  test :get do
    assert Binary.Dict.get(sample_dict, :foo)  == "bar"
    assert Binary.Dict.get(sample_dict, "foo") == "bar"

    assert Binary.Dict.get(sample_dict, :bar)  == nil
    assert Binary.Dict.get(sample_dict, "bar") == nil

    assert Binary.Dict.get(sample_dict, :bar, :other)  == :other
    assert Binary.Dict.get(sample_dict, "bar", :other) == :other
  end

  test :get! do
    assert Binary.Dict.get!(sample_dict, :foo)  == "bar"
    assert Binary.Dict.get!(sample_dict, "foo") == "bar"

    assert_raise KeyError, fn ->
      Binary.Dict.get!(sample_dict, :bar)
    end

    assert_raise KeyError, fn ->
      Binary.Dict.get!(sample_dict, "bar")
    end
  end

  test :put do
    dict = Binary.Dict.put(empty_dict, :foo, 1)
    assert Binary.Dict.get(dict, "foo") == 1

    dict = Binary.Dict.put(dict, "foo", 2)
    assert Binary.Dict.get(dict, :foo) == 2
  end

  test :put_new do
    dict = Binary.Dict.put_new(empty_dict, :foo, 1)
    assert Binary.Dict.get(dict, "foo") == 1

    dict = Binary.Dict.put_new(dict, "foo", 2)
    assert Binary.Dict.get(dict, :foo) == 1
  end

  test :size do
    assert Binary.Dict.size(empty_dict) == 0
    assert Binary.Dict.size(sample_dict) == 2
  end

  test :delete do
    dict = Binary.Dict.delete(sample_dict, :foo)
    assert Binary.Dict.get(dict, "foo") == nil

    dict = Binary.Dict.delete(sample_dict, "foo")
    assert Binary.Dict.get(dict, :foo) == nil

    dict = Binary.Dict.delete(sample_dict, :other)
    assert Binary.Dict.get(dict, :other) == nil
  end

  test :merge do
    dict = Binary.Dict.merge(sample_dict, [foo: 1, baz: 2])
    assert Binary.Dict.get(dict, "foo") == 1
    assert Binary.Dict.get(dict, :baz) == 2

    dict = Binary.Dict.merge(sample_dict, [foo: 1, baz: 2],
      fn(k, v1, v2) -> String.duplicate(k <> v1, v2) end)
    assert Binary.Dict.get(dict, "foo") == "foobar"
    assert Binary.Dict.get(dict, :baz) == "bazbatbazbat"
  end

  test :update do
    dict = Binary.Dict.update(sample_dict, :foo, fn(v) -> v <> v end)
    assert Binary.Dict.get(dict, "foo") == "barbar"

    dict = Binary.Dict.update(sample_dict, "foo", fn(v) -> v <> v end)
    assert Binary.Dict.get(dict, :foo) == "barbar"

    assert_raise KeyError, fn ->
      Binary.Dict.update(sample_dict, :other, fn(v) -> v <> v end)
    end
  end

  test :update_with_initial do
    dict = Binary.Dict.update(sample_dict, :foo, 0, fn(v) -> v <> v end)
    assert Binary.Dict.get(dict, "foo") == "barbar"

    dict = Binary.Dict.update(sample_dict, "foo", 0, fn(v) -> v <> v end)
    assert Binary.Dict.get(dict, :foo) == "barbar"

    dict = Binary.Dict.update(sample_dict, :other, 0, fn(v) -> v <> v end)
    assert Binary.Dict.get(dict, :other) == 0
  end

  defp empty_dict do
    Binary.Dict.new
  end

  defp sample_dict do
    Binary.Dict.new [foo: "bar", baz: "bat"]
  end
end
