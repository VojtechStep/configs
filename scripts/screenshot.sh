#!/bin/sh

scr_dir="$SCREENSHOT_DIR"

if [ -z "$scr_dir" ]; then
	scr_dir="$HOME/Screenshots"
fi

if ! [ -d "$scr_dir" ]; then
  mkdir -p "$scr_dir"
fi

gen_file_name(){
  win_name=${1:-screen}
	echo "$scr_dir/$(date +"%Y%m%d.%H%M%S").${win_name}.png"
}

take_screenshot_of_window(){
	# $1 - window ID
	# $2 - window name (for file name)
	win_id=$1
	win_name=$2

  file_loc=$(gen_file_name $win_name)

	if [ -z "$win_id" ]; then
		import $file_loc
	else
		import -window $1 $file_loc
	fi

  xclip -sel clip -t image/png < $file_loc
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
  filename)
    gen_file_name $2
    ;;
esac
