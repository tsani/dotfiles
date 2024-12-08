function conky_format(format, number)
  return string.format (format, conky_parse(number))
end

function conky_volume(foo)
  local handle = io.popen("wpctl get-volume @DEFAULT_AUDIO_SINK@ | cut -d ' ' -f2")
  local result = handle:read("*all")
  handle:close()
  return string.format("%3s%%", math.floor(100 * tonumber(result)))
end
