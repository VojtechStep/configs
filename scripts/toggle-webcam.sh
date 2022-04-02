#!/bin/sh

# list all flippers
flippers=$(pgrep -f ffmpeg-flipper-sl)

# if no flippers are running
if [ $? -ne 0 ]; then
  # run a flipper
  ffmpeg-flipper-sl -f v4l2 -i /dev/video0 -pix_fmt yuyv422 -vf hflip,vflip -f v4l2 /dev/video2 &
else # if there is already a flipper running
  # kill it
  kill $flippers
fi
