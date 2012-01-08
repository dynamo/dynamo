defmodule Dynamo::Router::Compiler do
  # Receives a merged branches as `gtg` and converts
  # them to a series of compiled functions injected
  # into the `target` module.
  def compile(module, gtg, counter // 0) do
    result = Enum.foldl gtg, counter + 1, compile_each(_, _, module, counter)
    Module.eval_quoted module, error_for(name_for(counter)), [], __FILE__, __LINE__
    result
  end

  # The endpoint is special cased because the second element
  # of the tuple is not a list. If we have an endpoint, it
  # means we just need to add the endpoint clause using the
  # verb and the extra parameters given.
  defp compile_each({ :endpoint, { verb, extra } }, counter, module, current) do
    name = name_for(current)

    contents = quote do
      defp unquote(name).(unquote(verb), [], dict) do
        { :ok, unquote(extra), dict }
      end
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
    counter + 1
  end

  # Each other item besides the endpoint is simply compiled
  # recursively.
  defp compile_each(item, counter, module, current) do
    { left, right } = flatten(item)

    name   = name_for(current)
    invoke = name_for(counter)

    Module.eval_quoted module, contents_for(name, left, invoke),
      [], __FILE__, __LINE__

    compile(module, right, counter)
  end

  # If left part is a literal with only one item and
  # the second item is also a literal, we can flatten
  # them to avoid extra method calls.
  defp flatten({ left, [{ right, args }] }) when is_list(left) andalso is_list(right) do
    flatten({ left ++ right, args })
  end

  defp flatten(tuple) do
    tuple
  end

  # Return the contents to be compiled according
  # to the left part. If left is a list, it means
  # we have a literal.
  defp contents_for(name, left, invoke) when is_list(left) do
    quote do
      defp unquote(name).(verb, unquote(left) ++ rest, dict) when is_list(rest) do
        unquote(invoke).(verb, rest, dict)
      end
    end
  end

  # Returns an error route. Used as last clause.
  defp error_for(name) do
    quote do
      defp unquote(name).(_, _, _), do: :error
    end
  end

  # Return an atom representing the name for the given counter.
  defp name_for(counter) do
    list_to_atom List.flatten(['_',integer_to_list(counter)|'_recognize'])
  end
end