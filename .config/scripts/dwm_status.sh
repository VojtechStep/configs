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

refresh_vol() {
	active=$(pacmd list-sinks | rg -w RUNNING -A7)
	if [ "$active" = "" ]; then
		active=$(pacmd list-sinks | rg -w volume -A4)
	fi
	muted=$(echo "$active" | rg -e "muted: (\w*)" -or "\$1")
	if [ "$muted" = "yes" ]; then
		vol="(mute)"
	else
		vol=$(echo "$active" | rg -e "volume: front-left.*?/\s*(\d{2,3}%)" -or "\$1")
	fi
}

# Updates all the blocks, then redraws
refresh_all(){
	refresh_kbd
	refresh_date
	refresh_bright
	refresh_batt
	refresh_essid
	refresh_vol
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

# RTMIN+4 -> refresh volume
trap "refresh_vol && redraw" RTMIN+4

# Start by refreshing all
refresh_all

while :
do
	# Sleep for 5 seconds while also receiving signals
	sleep 5 &
	wait $!
	refresh_periodic
done
