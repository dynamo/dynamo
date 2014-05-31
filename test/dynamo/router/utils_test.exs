defmodule Dynamo.Router.UtilsTest do
  require Dynamo.Router.Utils, as: R

  use ExUnit.Case, async: true

  @opts [context: Dynamo.Router.Utils]

  defp generate_match(route) do
    R.generate_match(route, Dynamo.Router.Utils)
  end

  test "root" do
    assert R.split("/") == []
    assert R.split("") == []
  end

  test "split single segment" do
    assert R.split("/foo") == ["foo"]
    assert R.split("foo") == ["foo"]
  end

  test "split with more than one segment" do
    assert R.split("/foo/bar") == ["foo", "bar"]
    assert R.split("foo/bar") == ["foo", "bar"]
  end

  test "split removes trailing slash" do
    assert R.split("/foo/bar/") == ["foo", "bar"]
    assert R.split("foo/bar/") == ["foo", "bar"]
  end

  test "generate match with literal" do
    assert quote(@opts, do: { [], ["foo"] }) == generate_match("/foo")
    assert quote(@opts, do: { [], ["foo"] }) == generate_match("foo")
  end

  test "generate match with identifier" do
    assert quote(@opts, do: { [:id], ["foo", id] }) == generate_match("/foo/:id")
    assert quote(@opts, do: { [:username], ["foo", username] }) == generate_match("foo/:username")
  end

  test "generate match with literal plus identifier" do
    assert quote(@opts, do: { [:id], ["foo", "bar-" <> id] }) == generate_match("/foo/bar-:id")
    assert quote(@opts, do: { [:username], ["foo", "bar" <> username] }) == generate_match("foo/bar:username")
  end

  test "generate match only with glob" do
    assert quote(@opts, do: { [:bar], bar }) == generate_match("*bar")
    assert quote(@opts, do: { [:glob], glob }) == generate_match("/*glob")

    assert quote(@opts, context: Dynamo.Router.Utils, do: { [:bar], ["id-" <> _ | _] = bar }) == generate_match("id-*bar")
    assert quote(@opts, do: { [:glob], ["id-" <> _ | _] = glob }) == generate_match("/id-*glob")
  end

  test "generate match with glob" do
    assert quote(@opts, do: { [:bar], ["foo" | bar] }) == generate_match("/foo/*bar")
    assert quote(@opts, do: { [:glob], ["foo" | glob] }) == generate_match("foo/*glob")
  end

  test "generate match with literal plus glob" do
    assert quote(@opts, do: { [:bar], ["foo" | ["id-" <> _ | _] = bar] }) == generate_match("/foo/id-*bar")
    assert quote(@opts, do: { [:glob], ["foo" | ["id-" <> _ | _] = glob] }) == generate_match("foo/id-*glob")
  end

  test "generate invalid match with segments after glob" do
    generate_match("/foo/*bar/baz")
    flunk "generate_match should have failed"
  rescue
    x in [Dynamo.Router.InvalidSpecError] ->
      "cannot have a *glob followed by other segments" = x.message
  end
end
