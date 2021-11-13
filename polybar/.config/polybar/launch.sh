#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar --reload mainbar 2>$HOME/logs/polybar-mainbar.log &

# load external bar in every non-primary monitor
# if type "xrandr"; then
#   for m in $(xrandr --query | grep --invert-match "primary" | grep " connected" | cut -d" " -f1); do
#     MONITOR=$m polybar --reload external 2>$HOME/logs/polybar-external-$m.log &
#   done
# else
#   polybar --reload external 2>$HOME/logs/polybar-external.log &
# fi


# Just use mainbar on each non-primary monitor
if type "xrandr"; then
  for m in $(xrandr --query | grep --invert-match "primary" | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload mainbar 2>$HOME/logs/polybar-external-$m.log &
  done
else
  polybar --reload mainbar 2>$HOME/logs/polybar-external.log &
fi

echo "Polybar launched..."
