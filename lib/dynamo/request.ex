defrecord Dynamo.Request.File, body: nil, name: nil, content_type: nil, filename: nil

defrecord Dynamo.Request.UnfetchedError, aspect: nil do
  def message(exception) do
    aspect = aspect(exception)
    "Did not fetch #{aspect} from request, add `fetch :#{aspect}` in order to access it"
  end
end