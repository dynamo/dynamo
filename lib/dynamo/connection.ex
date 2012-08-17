
defmodule Dynamo.Connection do
  defrecord File, body: nil, name: nil, content_type: nil, filename: nil

  defrecord UnfetchedError, aspect: nil do
    def message(exception) do
      aspect = aspect(exception)
      "Did not fetch #{aspect} from request, add `fetch :#{aspect}` in order to access it"
    end
  end

  def behaviour_info(:callbacks) do
    [ assign: 3,
      assigns: 1,
      cookies: 1,
      delete_cookie: 2,
      delete_cookie: 3,
      delete_resp_header: 2,
      fetch: 2,
      forward_to: 3,
      method: 1,
      params: 1,
      path: 1,
      path_info: 1,
      path_info_segments: 1,
      path_segments: 1,
      query_string: 1,
      req_cookies: 1,
      req_headers: 1,
      resp: 3,
      resp_body: 1,
      resp_cookies: 1,
      resp_headers: 1,
      script_name: 1,
      script_name_segments: 1,
      send: 1,
      send: 3,
      set_cookie: 3,
      set_cookie: 4,
      set_resp_header: 3,
      state: 1,
      status: 1,
      version: 1 ]
  end
end