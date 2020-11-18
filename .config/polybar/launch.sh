#!/usr/bin/env sh

## Add this to your wm startup file.

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar
polybar laptop &

my_laptop_external_monitor=$(polybar -m | cut -d ':' -f 1 | grep HDMI1)
if [[ $my_laptop_external_monitor = HDMI1 ]]; then
    polybar HDMI1 &
    polybar DP1 &
fi
