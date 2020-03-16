#!/usr/bin/env sh

scr_dir="$SCREENSHOT_DIR"

if [ "$scr_dir" = "" ]; then
	scr_dir="$HOME/Screenshots"
fi

take_screenshot_of_window(){
	# $1 - window ID
	# $2 - window name (for file name)
	win_id=$1
	win_name=$2

	if [ "$win_name" = "" ]; then
		win_name="screen"
	fi

	file_loc="$scr_dir/$(date +"%Y%m%d.%H%M%S").$win_name.png"
	if [ "$win_id" = "" ]; then
		import $file_loc
	else
		import -window $1 $file_loc
	fi
}

notify.sh "Taking a screenshot" &
case $1 in
	root)
		take_screenshot_of_window root
		;;
	current)
		id=$(xprop -root _NET_ACTIVE_WINDOW | rg -e "# (\w+)" -or "\$1")
		win_name=$(xprop WM_CLIENT_LEADER -id $id | rg -e "# (\w+)" -or "\$1" | xargs xprop WM_CLASS -id | rg -e "= (?:(?:\"\w*\",\s*)*)\s*\"(\w+)\"" -or "\$1")
		take_screenshot_of_window $id $win_name
		;;
	select)
		take_screenshot_of_window
		;;
esac
