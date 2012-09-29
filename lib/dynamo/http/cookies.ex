defmodule Dynamo.HTTP.Cookies do
  

  def cookie_header(key, value, options // []) do
    key = URI.encode(key)
    value = URI.encode(value)
    header = "#{key}=#{value}"

    if path = options[:path] do
      header = header <> "; path=#{path}"
    end

    if domain = options[:domain] do
      header = header <> "; domain=#{domain}"
    end

    if max_age = options[:max_age] do
      time = options[:universal_time] || :calendar.universal_time
      time = add_seconds(time, max_age)
      header = header <> "; expires=#{rfc2822(time)}"
    end

    if options[:secure] do
      header = header <> "; secure"
    end

    unless options[:http_only] == false do
      header = header <> "; HttpOnly"
    end

    header
  end

  defp rfc2822({ { year, month, day } = date, { hour, minute, second } }) do
    weekday = weekday_name(:calendar.day_of_the_week(date))
    month_name = month_name(month)
    padded_day = pad(day)
    padded_hour = pad(hour)
    padded_minute = pad(minute)
    padded_second = pad(second)

    "#{weekday}, #{padded_day}-#{month_name}-#{year} #{padded_hour}:#{padded_minute}:#{padded_second} GMT"
  end

  defp pad(number) when number in 1..9 do
    "0#{number}"
  end

  defp pad(number) do
    to_binary(number)
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