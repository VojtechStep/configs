# Defined in /tmp/fish.DV4g17/f.fish @ line 2
function f --argument url
	if string match -r -q '\.svg$' $url
    set conversion --conversion-timeout 2
  end
	if string match -r -q '^https?://' $url
		curl -s $url --output - | feh $conversion - 2>/dev/null 1>/dev/null &
		disown
	else
		feh {$conversion} $url 2>/dev/null 1>/dev/null &
		disown
	end
end
