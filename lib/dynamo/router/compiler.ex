defmodule Dynamo::Router::Compiler do
  # Receives merged branches as `gtg` and converts them to a
  # series of compiled functions injected into the given module.
  def compile(module, gtg, counter // 0) do
    { contents, result } = Enum.map_reduce gtg, counter + 1, compile_each(&1, &2, module, counter)
    contents = contents ++ [error_for(name_for(counter))]
    Module.eval_quoted module, contents, [], __FILE__, __LINE__
    result
  end

  # The endpoint is special cased because the second element
  # of the tuple is not a list. If we have an endpoint, it
  # means we just need to add the Final clause using the
  # verb and the extra parameters given.
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

  # If the left part only points to another item
  # and both of them are literals, we can concatenate
  # them together to avoid matching.
  defp flatten({ left, [{ right, args }] }) when is_list(left) andalso is_list(right) do
    flatten({ left ++ right, args })
  end

  defp flatten(tuple) do
    tuple
  end

  # Returns an error route. Used as last clause.
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