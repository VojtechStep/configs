#!/bin/sh

state="running"
date=""
kbd=""
bri=""
batt=""
essid=""
vol=""
vol_icon=""

# All refresh_* functions only update internal state,
# they do not redraw the status bar.
# To redraw the status bar, use the "redraw" function

# Updates the date block
refresh_date(){
  date=$(date +%H:%M)
}

# Updates the keyboard layout block
refresh_kbd(){
  kbd=$(xkblayout-state print "%e")
}

# Updates the brightness block
refresh_bright(){
  bri=$(echo "$(cat ~/.config/brightness)*100/1" | bc)
}

# Updates the battery block
refresh_batt() {
  batt=$(acpi -b | cut -d" " -f4 | tr -d ",")
}

# Updates the network ESSID block
refresh_essid() {
  essid=$(iwctl station wlan0 get-networks | rg '\x1b\[1;30m> \x1b\[0m(\s?.*?)\s+(psk|8021x|open)' -or '$1')
  if [ "$essid" = "" ]; then
    essid="No Internet"
  fi
}

refresh_vol() {
  active=$(pacmd list-sinks 2>/dev/null | rg -w RUNNING -A7)
  if [ "$active" = "" ]; then
    vol="婢"
    # active=$(pacmd list-sinks | rg -w volume -A4)
  else
    muted=$(echo "$active" | rg -e "muted: (\w*)" -or "\$1")
    if [ "$muted" = "yes" ]; then
      vol="婢"
    else
      vol=$(echo "$active" | rg -e "volume: front-left.*?/\s*(\d{1,3})%" -or "\$1")
      if [ "$vol" -gt 80 ]; then
        vol_icon="墳"
      elif [ "$vol" -gt 40 ]; then
        vol_icon="奔"
      else
        vol_icon="奄"
      fi
      vol="$vol_icon $vol%"
    fi
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
}

# Called every major tick (by default every 5s)
refresh_periodic(){
  refresh_date
  refresh_batt
  redraw
}

# Performs the status bar redraw
redraw(){
  if [ "$state" = "running" ]; then
    xsetroot -name " $kbd |  $bri | $vol |  $essid |  $batt | $date "
  fi
}

# RMIN+1 -> refresh all
trap "refresh_all" RTMIN+1

# RTMIN+2 -> refresh keyboard
trap "refresh_kbd" RTMIN+2

# RTMIN+3 -> refresh brightness
trap "refresh_bright" RTMIN+3

# RTMIN+4 -> refresh volume
trap "refresh_vol" RTMIN+4

trap "state=paused" RTMIN+5

trap "state=running" RTMIN+6

# Start by refreshing all
refresh_all
redraw

while :
do
  # Sleep for a minute while also receiving signals
  sleep 1m &
  wait $!
  refresh_periodic
done
