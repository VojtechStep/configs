#! /bin/sh

# xrandr --dpi 315
# xrandr --output DP1 --auto --primary --scale 2x2 --output eDP1 --pos 7680x0 --rotate inverted
xrandr --dpi 96

MON_LAYOUT=${1:-auto}

if [ $MON_LAYOUT = auto ]; then
  MON_COUNT=$(xrandr --query | rg " connected" -c)
  if [ $MON_COUNT -eq 1 ]; then
    MON_LAYOUT=single
  elif [ $MON_COUNT -eq 2 ]; then
    # xrandr --output DP1 --auto --primary --pos 0x0 --output eDP1 --mode 1920x1080 --pos 3840x0
    xrandr --output DP1 --auto --primary --pos 1920x0 --output eDP1 --mode 1920x1080 --pos 0x0 --rotate inverted
  else
    echo Unsupported number of monitors: $MON_COUNT
  fi
fi

case $MON_LAYOUT in
  double-invert) # home setup, laptop to the left
    xrandr --output eDP1 \
           --mode 1920x1080 --pos 0x0 --rotate inverted \
           --output DP1 \
           --auto --primary --pos 1920x0
    wacom_setup.sh ultrawide
  ;;
  double-normal) # home setup, laptop to the right
    xrandr --output eDP1 \
           --mode 1920x1080 --pos 3840x0 \
           --output DP1 \
           --auto --primary --pos 0x0
    wacom_setup.sh ultrawide
  ;;
  double-lab) # lab setup, laptop is primary
    xrandr --output eDP1 \
           --mode 1920x1080 --pos 0x0 --rotate normal --primary \
           --output DP1 \
           --auto --pos 1920x0
  ;;
  duplicate) # lab setup, duplicating screen
    xrandr --output eDP1 \
           --mode 1920x1080 --rotate normal --primary \
           --output DP1 \
           --same-as eDP1
    wacom_setup.sh single
  ;;
  single) # on-the-go setup
    xrandr --output eDP1 \
           --mode 1920x1080 --primary --rotate normal
    wacom_setup.sh single
  ;;
  *)
    echo "Unsupported monitor layout: $MIN_LAYOUT"
  ;;
esac

systemctl --user start fehbg
