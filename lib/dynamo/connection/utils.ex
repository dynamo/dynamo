defmodule Dynamo.Connection.Utils do
  @moduledoc false

  @doc """
  Create a temporary directory usually
  used to store uploaded files.
  """
  def tmp_dir do
    { mega, _, _ } = :erlang.now
    dir = "dynamo-#{mega}"

    write_env_tmp_dir('TMPDIR', dir) ||
      write_env_tmp_dir('TMP', dir)  ||
      write_env_tmp_dir('TEMP', dir) ||
      write_tmp_dir("/tmp/" <> dir)  ||
      write_tmp_dir(Path.expand(dir)) ||
      raise "cannot create temporary directory"
  end

  defp write_env_tmp_dir(env, dir) do
    case System.get_env(env) do
      nil -> nil
      tmp -> write_tmp_dir Path.join(tmp, dir)
    end
  end

  defp write_tmp_dir(dir) do
    case File.mkdir_p(dir) do
      :ok -> dir
      { :error, _ } -> nil
    end
  end

  @doc """
  Creates a file with random name at the given temporary
  directory. It returns the name of the file and the result
  of the executed callback as a tuple.

  In case the file could not be created after 10 attemps,
  it raises an exception.
  """
  @max_attempts 10

  def random_file(prefix, tmp_dir, callback) do
    random_file(prefix, tmp_dir, callback, 0)
  end

  defp random_file(prefix, tmp_dir, callback, attempts) when attempts < @max_attempts do
    { mega, sec, mili } = :erlang.now()
    name = Path.join(tmp_dir, "#{prefix}-#{mega}-#{sec}-#{mili}")
    case :file.open(name, [:write, :exclusive, :binary]) do
      { :error, :eaccess } -> random_file(prefix, tmp_dir, callback, attempts + 1)
      { :ok, file } ->
        result = try do
          callback.(file)
        after
          :file.close(file)
        end
        { name, result }
    end
  end

  defp random_file(_, tmp_dir, _, attempts) do
    raise "Could not create random file at #{tmp_dir} after #{attempts} attempts. What gives?"
  end

  @doc """
  Receives response headers and cookies {key, value, options} list and returns
  merged headers with cookies headers.
  """
  def merge_resp_headers(headers, cookies) do
    Enum.reduce cookies, Binary.Dict.to_list(headers), fn({ key, value, opts }, acc) ->
      [{ "set-cookie", cookie_header(key, value, opts) }|acc]
    end
  end

  @doc """
  Receives a cookie key, value, options and returns
  a cookie header.
  """
  def cookie_header(key, value, options // [])

  def cookie_header(key, nil, options) do
    cookie_header(key, "", options)
  end

  def cookie_header(key, value, options) do
    header = "#{key}=#{value}; path=#{Keyword.get(options, :path, "/")}"

    if domain = options[:domain] do
      header = header <> "; domain=#{domain}"
    end

    if max_age = options[:max_age] do
      time = options[:universal_time] || :calendar.universal_time
      time = add_seconds(time, max_age)
      header = header <> "; expires=" <> rfc2822(time) <> "; max-age=" <> integer_to_binary(max_age)
    end

    if options[:secure] do
      header = header <> "; secure"
    end

    unless options[:http_only] == false do
      header = header <> "; HttpOnly"
    end

    header
  end

  defp pad(number) when number in 0..9 do
    << ?0, ?0 + number >>
  end

  defp pad(number) do
    integer_to_binary(number)
  end

  defp rfc2822({ { year, month, day } = date, { hour, minute, second } }) do
    weekday_name  = weekday_name(:calendar.day_of_the_week(date))
    month_name    = month_name(month)
    padded_day    = pad(day)
    padded_hour   = pad(hour)
    padded_minute = pad(minute)
    padded_second = pad(second)
    binary_year   = integer_to_binary(year)

    weekday_name <> ", " <> padded_day <>
      " " <> month_name <> " " <> binary_year <>
      " " <> padded_hour <> ":" <> padded_minute <>
      ":" <> padded_second <> " GMT"
  end

  defp weekday_name(1), do: "Mon"
  defp weekday_name(2), do: "Tue"
  defp weekday_name(3), do: "Wed"
  defp weekday_name(4), do: "Thu"
  defp weekday_name(5), do: "Fri"
  defp weekday_name(6), do: "Sat"
  defp weekday_name(7), do: "Sun"

  defp month_name(1),  do: "Jan"
  defp month_name(2),  do: "Feb"
  defp month_name(3),  do: "Mar"
  defp month_name(4),  do: "Apr"
  defp month_name(5),  do: "May"
  defp month_name(6),  do: "Jun"
  defp month_name(7),  do: "Jul"
  defp month_name(8),  do: "Aug"
  defp month_name(9),  do: "Sep"
  defp month_name(10), do: "Oct"
  defp month_name(11), do: "Nov"
  defp month_name(12), do: "Dec"

  defp add_seconds(time, seconds_to_add) do
    time_seconds = :calendar.datetime_to_gregorian_seconds(time)
    :calendar.gregorian_seconds_to_datetime(time_seconds + seconds_to_add)
  end
end