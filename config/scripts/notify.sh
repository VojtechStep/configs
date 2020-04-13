#!/bin/sh

text=$1
duration=$2

if [ "$1" = "" ]; then
	echo "Usage:"
	echo "	notify [text] [duration]"
	echo
	echo "	text"
	echo "		Text to display"
	echo "	duration"
	echo "		For how long to display the text, in seconds"
	exit 1
fi

if [ "$duration" = "" ]; then
	duration=3
fi

if [ "$(echo "1 * $duration" | bc)" != "$duration" ]; then
	echo "Unknown duration: $duration"
	exit 1
fi

pkill -RTMIN+5 -f "sh .*dwm_status"

xsetroot -name " $text "
sleep $duration &
wait $!

pkill -RTMIN+6 -f "sh .*dwm_status"
