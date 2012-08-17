Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Test.ConnectionTest do
  use ExUnit.Case, async: true

  alias Dynamo.Test.Connection, as: C

  test :path_segments do
    assert conn(:GET, "/foo/bar").path_segments == ["foo", "bar"]
    assert conn(:GET, "/").path_segments == []

    assert conn(:GET, "/foo/bar").path == "/foo/bar"
    assert conn(:GET, "/").path == "/"
  end

  ## Misc

  test :forward_to do
    conn = conn(:GET, "/forward_to/foo/bar/baz")
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    conn = conn.forward_to [], Foo
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    conn = conn.forward_to ["foo", "bar", "baz"], Foo

    assert conn.path_info == "/foo/bar/baz"
    assert conn.path_info_segments == ["foo", "bar", "baz"]

    assert conn.path == "/forward_to/foo/bar/baz"
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert conn.script_name == "/forward_to"
    assert conn.script_name_segments == ["forward_to"]

    conn = conn.forward_to ["bar", "baz"], Bar

    assert conn.path_info == "/bar/baz"
    assert conn.path_info_segments == ["bar", "baz"]

    assert conn.path == "/forward_to/foo/bar/baz"
    assert conn.path_segments == ["forward_to", "foo", "bar", "baz"]

    assert conn.script_name == "/forward_to/foo"
    assert conn.script_name_segments == ["forward_to", "foo"]

    conn
  end

  defp conn(verb, path) do
    C.new.req(verb, path)
  end
end