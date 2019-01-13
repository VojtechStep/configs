function z -a 'url'
	curl -s $url --output - | zathura - &
end
