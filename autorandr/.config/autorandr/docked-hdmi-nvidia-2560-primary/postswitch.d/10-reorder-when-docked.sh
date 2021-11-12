#! /bin/bash

. "$SCRIPTS"/monitors.sh
IFS=" " read -r -a connected_monitors <<< "$(get_connected_monitors)"
echo "connected monitors: ${connected_monitors[*]}"

"$SCRIPTS"/bspwm_reorder "${connected_monitors[@]}"  

sleep 1

"$HOME"/.config/polybar/launch.sh
