function z -a 'url'
	if string match -r -q '^https?://' $url
		curl -s $url --output - | zathura - &
		disown
	else
		zathura $url &
		disown
	end
end
