Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.UtilsTest do
  require Dynamo.Router.Utils, as: R

  use ExUnit.Case, async: true

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
    assert quote(hygiene: [vars: false], do: { [], ["foo"] }) == R.generate_match("/foo")
    assert quote(hygiene: [vars: false], do: { [], ["foo"] }) == R.generate_match("foo")
  end

  test "generate match with identifier" do
    assert quote(hygiene: [vars: false], do: { [:id], ["foo", id] }) == R.generate_match("/foo/:id")
    assert quote(hygiene: [vars: false], do: { [:username], ["foo", username] }) == R.generate_match("foo/:username")
  end

  test "generate match with literal plus identifier" do
    assert quote(hygiene: [vars: false], do: { [:id], ["foo", "bar-" <> id] }) == R.generate_match("/foo/bar-:id")
    assert quote(hygiene: [vars: false], do: { [:username], ["foo", "bar" <> username] }) == R.generate_match("foo/bar:username")
  end

  test "generate match only with glob" do
    assert quote(hygiene: [vars: false], do: { [:bar], bar }) == R.generate_match("*bar")
    assert quote(hygiene: [vars: false], do: { [:glob], glob }) == R.generate_match("/*glob")

    assert quote(hygiene: [vars: false], do: { [:bar], ["id-" <> _ | _] = bar }) == R.generate_match("id-*bar")
    assert quote(hygiene: [vars: false], do: { [:glob], ["id-" <> _ | _] = glob }) == R.generate_match("/id-*glob")
  end

  test "generate match with glob" do
    assert quote(hygiene: [vars: false], do: { [:bar], ["foo" | bar] }) == R.generate_match("/foo/*bar")
    assert quote(hygiene: [vars: false], do: { [:glob], ["foo" | glob] }) == R.generate_match("foo/*glob")
  end

  test "generate match with literal plus glob" do
    assert quote(hygiene: [vars: false], do: { [:bar], ["foo" | ["id-" <> _ | _] = bar] }) == R.generate_match("/foo/id-*bar")
    assert quote(hygiene: [vars: false], do: { [:glob], ["foo" | ["id-" <> _ | _] = glob] }) == R.generate_match("foo/id-*glob")
  end

  test "generate invalid match with segments after glob" do
    R.generate_match("/foo/*bar/baz")
    flunk "generate_match should have failed"
  rescue
    x in [Dynamo.Router.InvalidSpecError] ->
      "cannot have a *glob followed by other segments" = x.message
  end
end
