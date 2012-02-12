defmodule Dynamo::Router::Compiler do
  # Main entry point for compilation, called recursively
  # during the compilation process. It works with two counters,
  # one indicates the name of the function where the current route
  # is being compiled and the other indicates the name to where the
  # current route should point to.
  #
  # All the functions are compiled straight into the given module.
  def compile(module, gtg, counter // 0) do
    { contents, result } = Enum.map_reduce gtg, counter + 1, compile_each(&1, &2, module, counter)
    contents = contents ++ [error_for(name_for(counter))]
    Module.eval_quoted module, contents, [], __FILE__, __LINE__
    result
  end

  # The endpoint is a special node and needs to be special cased.
  # In particular, the endpoint does not point to other nodes
  # and should just return the final result.
  defp compile_each({ :endpoint, { verb, extra } }, counter, _module, current) do
    name = name_for(current)

    contents = quote do
      defp unquote(name).(unquote(verb), [], dict) do
        { :ok, unquote(extra), dict }
      end
    end

    { contents, counter + 1 }
  end

  # Each other item besides the endpoint is simply compiled recursively.
  # Those other items are first flattened (allowing us to remove hops
  # in the matching algorithm) and then compiled according to the
  # `contents_for` function.
  defp compile_each(item, counter, module, current) do
    { left, right } = flatten(item)

    name   = name_for(current)
    invoke = name_for(counter)

    {
      contents_for(name, left, invoke),
      compile(module, right, counter)
    }
  end

  # Return the contents to be compiled according
  # to the current node (left part).

  # This handles a literal, e.g. 'foo'
  defp contents_for(name, left, invoke) when is_list(left) do
    quote do
      defp unquote(name).(verb, unquote(left) ++ rest, dict) when is_list(rest) do
        unquote(invoke).(verb, rest, dict)
      end
    end
  end

  # If the current node is a literal (like 'foo') and it points
  # to another single literal node (like 'bar'), we can simply
  # concatenate them now and avoid extra hops when doing the
  # recognition.
  defp flatten({ left, [{ right, args }] }) when is_list(left) andalso is_list(right) do
    flatten({ left ++ right, args })
  end

  defp flatten(tuple) do
    tuple
  end

  # Returns an error route. Used as last clause so we
  # return an atom :error instead of raising an exception.
  defp error_for(name) do
    quote do
      defp unquote(name).(_, _, _), do: :error
    end
  end

  # Return an atom representing the name for the given counter.
  defp name_for(counter) do
    :"_recognize_route_#{counter}"
  end
end