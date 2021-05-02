#! /bin/bash

# Get connected monitors from xrandr in a format like 'eDP-1-1 HDMI-0'
function get_connected_monitors() 
{
    primary_monitor=$(xrandr --query | grep "primary" | grep "connected" | cut -d" " -f1 )
    monitors=( "$primary_monitor" )

    # add non-primary connected monitors to array
    for m in $(xrandr --query | grep " connected" | grep --invert-match "primary" | cut -d" " -f1); do
        monitors[${#monitors[@]}]=$m
    done

    echo "${monitors[*]}"
}
