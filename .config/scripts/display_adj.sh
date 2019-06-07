#! /usr/bin/sh

INCREMENT=0.05
CONFIG=$HOME/.brightness

if [ -f "$CONFIG" ]; then
	LEVEL=$(cat $CONFIG)
else
	LEVEL=1
fi

adjust(){
	xrandr | rg -w connected | cut -d" " -f1 | xargs -L1 -I{} xrandr --output {} --brightness $LEVEL
	echo $LEVEL > $CONFIG
}

case $1 in
	+)
		NEW_LEVEL=$(echo "scale=2; $LEVEL + $INCREMENT" | bc)
		if [ "$(echo "$NEW_LEVEL <= 1" | bc)" -eq 1 ]; then
			LEVEL=$NEW_LEVEL
			adjust
		else
			exit 1
		fi
		;;
	-)
		NEW_LEVEL=$(echo "scale=2; $LEVEL - $INCREMENT" | bc)
		if [ "$(echo "$NEW_LEVEL >= 0.1" | bc)" -eq 1 ]; then
			LEVEL=$NEW_LEVEL
			adjust
		else
			exit 1
		fi
		;;
	=)
		adjust
		;;
esac

