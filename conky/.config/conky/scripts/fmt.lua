function conky_format(format, number)
  return string.format (format, conky_parse(number))
end

function conky_volume(foo)
  local handle = io.popen("pacmd list-sinks | grep 'volume: front-left:' | grep -o '[[:digit:]]*%' | head -n 1")
  local result = handle:read("*all"):gsub("%s+$", "")
  handle:close()
  return string.format("%4s", result)
end
