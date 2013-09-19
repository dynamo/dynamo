# This debug page is based on Better Errors, under MIT LICENSE.
#
# Copyright (c) 2012 Charlie Somerville
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

defmodule Dynamo.Filters.Exceptions.Debug do
  defrecord Frame, [:module, :function, :file, :line, :context, :index, :snippet, :link]

  import Dynamo.Helpers.Escaping

  def service(conn) do
    { status, kind, value, stacktrace } = conn.assigns[:exception]

    assigns = [
      frames: frames(conn, stacktrace),
      title: title(kind, value),
      message: message(kind, value)
    ]

    conn.send(status, template(conn, assigns))
  end

  defp title(:error, value), do: inspect value.__record__(:name)
  defp title(other, _),      do: "unhandled #{other}"

  defp message(:error, value), do: value.message
  defp message(_, value),      do: inspect value

  # Frames

  defp frames(conn, stacktrace) do
    dynamo  = conn.main
    root    = dynamo.root <> "/"
    config  = dynamo.config[:dynamo]
    editor  = config[:exceptions_editor]
    sources = config[:source_paths] ++ config[:templates_paths]
    sources = lc source inlist sources, path inlist Path.wildcard(Path.join(root, source)), do: path

    Enum.map_reduce(stacktrace, 0, &each_frame(&1, &2, root, sources, editor)) |> elem(0)
  end

  defp each_frame({ module, function, args_or_arity, opts }, index, root, sources, editor) do
    { mod, fun } = mod_fun(module, function, args_or_arity)
    { file, line } = { to_string(opts[:file] || "nofile"), opts[:line] }
    { relative, context, snippet } = file_context(file, line, root, sources)

    { Frame[
        module: mod,
        function: fun,
        file: relative,
        line: line,
        context: context,
        snippet: snippet,
        index: index,
        link: editor_link(file, line, editor)
      ], index + 1 }
  end

  def mod_fun(module, :__MODULE__, 0) do
    { inspect(module) <> " (module)", nil }
  end

  def mod_fun(_module, :__MODULE__, 2) do
    { "(module)", nil }
  end

  def mod_fun(_module, :__FILE__, 2) do
    { "(file)", nil }
  end

  def mod_fun(module, fun, args_or_arity) do
    { inspect(module), "#{fun}/#{arity(args_or_arity)}" }
  end

  defp arity(args) when is_list(args), do: length(args)
  defp arity(arity) when is_integer(arity), do: arity

  defp editor_link("/" <> _ = file, line, editor) when not nil?(editor) do
    editor = :binary.replace(editor, "__FILE__", URI.encode(file))
    editor = :binary.replace(editor, "__LINE__", to_string(line))
    h(editor)
  end

  defp editor_link(_, _, _), do: nil

  @radius 5

  defp file_context(original, line, root, sources) do
    file = Path.relative_to(original, root)

    if is_dynamo_source?(original, sources) do
      context = :dynamo
      snippet = is_integer(line) and extract_snippet(original, line)
    else
      context = :all
    end

    { file, context, snippet }
  end

  defp is_dynamo_source?(original, sources) do
    Enum.any? sources, fn(source) ->
      match? { 0, _ }, :binary.match(original, source)
    end
  end

  def extract_snippet(original, line) do
    if File.regular?(original) do
      to_discard = max(line - @radius - 1, 0)
      lines = File.stream!(original) |> Stream.take(line + 5) |> Stream.drop(to_discard)

      { first_five, lines } = Enum.split(lines, line - to_discard - 1)
      first_five = with_line_number first_five, to_discard + 1, false

      { center, last_five } = Enum.split(lines, 1)
      center = with_line_number center, line, true
      last_five = with_line_number last_five, line + 1, false

      first_five ++ center ++ last_five
    end
  end

  defp with_line_number(lines, initial, highlight) do
    Enum.map_reduce(lines, initial, fn(line, acc) ->
      { { acc, line, highlight }, acc + 1 }
    end) |> elem(0)
  end

  ## Templates

  require EEx

  EEx.function_from_file :defp, :template,
    Path.expand("../template.eex", __FILE__), [:conn, :assigns]
end
