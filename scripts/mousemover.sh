#!/bin/sh
#requires:
# 'xprintidle' for inactivity check (in ms)
# 'rand' for generating random number (screen resolution)
# 'xdotool' to move the mouse pointer

#parameters:
# 100000 idle time in ms before executing the mousemove
# 800 / 600: your screen resolution, at at least the moving range for the mouse pointer

while :; do
    if  [ $(xprintidle) -gt 100000 ]
    then
        xdotool mousemove `rand -M 800` `rand -M 600`;
    fi

    sleep 30
done
