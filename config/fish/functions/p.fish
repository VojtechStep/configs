# Defined in /tmp/fish.rwgxli/z.fish @ line 2
function p --argument url
	if string match -r -q '^https?://' $url
		curl -s $url --output - | zathura 2>/dev/null 1>/dev/null - &
		disown
    return
	else if test -r $url
		zathura $url 2>/dev/null 1>/dev/null &
		disown
    return
  else
    echo File not readable >&2
    return 1
	end
end
