#!/usr/bin/env sh

date=""
kbd=""
bri=""
batt=""
essid=""

# All refresh_* functions only update internal state,
# they do not redraw the status bar.
# To redraw the status bar, use the "redraw" function

# Updates the date block
refresh_date(){
	date=$(date +%T)
}

# Updates the keyboard layout block
refresh_kbd(){
	kbd=$(xkblayout-state print "%s")
}

# Updates the brightness block
refresh_bright(){
	bri=$(echo "$(cat ~/.brightness)*100/1" | bc)
}

# Updates the battery block
refresh_batt() {
	batt=$(acpi -b | cut -d" " -f4 | tr -d ",")
}

# Updates the network ESSID block
refresh_essid() {
	essid=$(nmcli | rg -e "connected to (.*)$" -or "\$1")
	if [ "$essid" = "" ]; then
		essid="No Internet"
	fi
}

# Updates all the blocks, then redraws
refresh_all(){
	refresh_kbd
	refresh_date
	refresh_bright
	refresh_batt
	refresh_essid
	redraw
}

# Called every major tick (by default every 5s)
refresh_periodic(){
	refresh_date
	refresh_batt
	redraw
}

# Performs the status bar redraw
redraw(){
	xsetroot -name " $kbd | ☀️ $bri | 🔉 $vol |  $essid | $batt | $date "
}

# RMIN+1 -> refresh all
trap "refresh_all && redraw" RTMIN+1

# RTMIN+2 -> refresh keyboard
trap "refresh_kbd && redraw" RTMIN+2

# RTMIN+3 -> refresh brightness
trap "refresh_bright && redraw" RTMIN+3

# Start by refreshing all
refresh_all

while :
do
	# Sleep for 5 seconds while also receiving signals
	sleep 5 &
	wait $!
	refresh_periodic
done
