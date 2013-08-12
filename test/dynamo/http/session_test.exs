defmodule Dynamo.HTTP.SessionTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case
  import Dynamo.HTTP.Session

  test :get_session do
    conn = conn(:GET, "/").put_private(:dynamo_session, [test_get_session: "123"])
    assert get_session(conn, :test_no_such_session) == nil
    assert get_session(conn, :test_get_session) == "123"
  end

  test :del_session do
    conn = conn(:GET, "/").put_private(:dynamo_session, [test_del_session: "123"])
    conn = conn.put_private(:dynamo_session_opts, {0, false, []})
    
    assert get_session(conn, :test_del_session) == "123"
    conn = del_session(conn, :test_del_session)
    assert get_session(conn, :test_del_session) == nil

  end


end
