#!/bin/sh

laptop_monitor=eDP-1-1
second_monitor=DP-1

if xrandr | grep "$second_monitor disconnected"; then
	xrandr --output "$second_monitor" --off --output "$laptop_monitor" --auto
elif xrandr | grep "$laptop_monitor disconnected"; then
	xrandr --output "$laptop_monitor" --off --output "$second_monitor" --auto
else
	xrandr --output "$second_monitor" --mode 2560x1080 --pos 1920x0 --output "$laptop_monitor" --primary --mode 1920x1080 --pos 0x0  
fi
