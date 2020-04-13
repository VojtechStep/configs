function z -a 'url'
	if string match -r -q '^https?://' $url
		curl -s $url --output - | zathura 2>/dev/null 1>/dev/null - &
		disown
	else
		zathura $url 2>/dev/null 1>/dev/null &
		disown
	end
end
