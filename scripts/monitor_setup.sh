#! /bin/sh

# xrandr --dpi 315
# xrandr --output DP1 --auto --primary --scale 2x2 --output eDP1 --pos 7680x0 --rotate inverted
xrandr --dpi 96

LAPTOP_MON=eDP1
EXTERNAL_MON=DP1

MON_LAYOUT=${1:-auto}

if [ $MON_LAYOUT = auto ]; then
  MON_COUNT=$(xrandr --query | rg " connected" -c)
  if [ $MON_COUNT -eq 1 ]; then
    MON_LAYOUT=single
  elif [ $MON_COUNT -eq 2 ]; then
    # xrandr --output DP1 --auto --primary --pos 0x0 --output eDP1 --mode 1920x1080 --pos 3840x0
    # xrandr --output DP1 --auto --primary --pos 1920x0 --output eDP1 --mode 1920x1080 --pos 0x0 --rotate inverted
    MON_LAYOUT=double-above
    # MON_LAYOUT=double-work
  else
    echo Unsupported number of monitors: $MON_COUNT
  fi
fi

case $MON_LAYOUT in
  --list-modes) # keep in sync with the accepted modes below + auto
    echo auto
    echo single
    echo double-normal
    echo double-work
    echo double-invert
    echo double-above
    echo double-lab
    echo duplicate
    exit 0
  ;;
  double-invert) # home setup, laptop to the left
    xrandr --output $LAPTOP_MON \
           --mode 1920x1080 --pos 0x0 --rotate inverted \
           --output $EXTERNAL_MON \
           --auto --primary --pos 1920x0
    wacom_setup.sh ultrawide
  ;;
  double-normal) # home setup, laptop to the right
    xrandr --output $LAPTOP_MON \
           --mode 1920x1080 --pos 0x0 --rotate normal \
           --output $EXTERNAL_MON \
           --auto --primary --pos 1920x0
    wacom_setup.sh ultrawide
  ;;
  double-above)
    xrandr --output $LAPTOP_MON \
           --mode 1920x1080 --pos 960x1200 --rotate normal \
           --output $EXTERNAL_MON \
           --auto --primary --pos 0x0
  ;;
  double-work)
    xrandr --output $LAPTOP_MON \
           --mode 1920x1080 --pos 960x1440 --rotate normal \
           --output $EXTERNAL_MON \
           --mode 3440x1440 --primary --pos 0x0
  ;;
  double-lab) # lab setup, laptop is primary
    xrandr --output $LAPTOP_MON \
           --mode 1920x1080 --pos 0x0 --rotate normal --primary \
           --output $EXTERNAL_MON \
           --auto --pos 1920x0
  ;;
  duplicate) # lab setup, duplicating screen
    xrandr --output $LAPTOP_MON \
           --mode 1920x1080 --rotate normal --primary \
           --output $EXTERNAL_MON \
           --same-as $LAPTOP_MON
    wacom_setup.sh single
  ;;
  single) # on-the-go setup
    xrandr --output $LAPTOP_MON \
           --mode 1920x1080 --primary --rotate normal
    wacom_setup.sh single
  ;;
  *)
    echo "Unsupported monitor layout: $MIN_LAYOUT"
  ;;
esac

systemctl --user start fehbg
