defmodule Dynamo.HTTP.SessionTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case
  import Dynamo.HTTP.Session

  test :get_session do
    conn = conn(:GET, "/").put_private(:dynamo_session, [test_get_session: "123"])
    assert get_session(conn, :test_no_such_session) == nil
    assert get_session(conn, :test_get_session) == "123"
  end

  test :delete_session do
    conn = conn(:GET, "/").put_private(:dynamo_session, [test_delete_session: "123"])
    conn = conn.put_private(:dynamo_session_opts, {0, false, []})
    
    assert get_session(conn, :test_delete_session) == "123"
    conn = delete_session(conn, :test_delete_session)
    assert get_session(conn, :test_delete_session) == nil

  end


end
