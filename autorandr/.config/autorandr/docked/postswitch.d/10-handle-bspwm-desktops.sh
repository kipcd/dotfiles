#! /bin/bash

echo "autorandr postswitch" > $HOME/logs/autorandr.log

number_of_desktops=$(bspc query -D | wc -l)
number_of_monitors=$(bspc query -M | wc -l)

echo "desktops: $number_of_desktops"
echo "monitors: $number_of_monitors"

desktop_index=$((number_of_desktops / number_of_monitors))
readarray -t desktops < <( bspc query -D )
echo "${desktops[@]}" 

readarray -t monitors < <( bspc querly -M )
echo "${monitors[@]}"

for (( m=0; m<$number_of_monitors; m++ ))
do 
    echo "${monitors[$m]}"
done
for (( i=$desktop_index; i<$number_of_desktops; i++ ))
do
    echo -n "${desktops[$i]}"
    bspc desktop ${desktops[$i]} --to-monitor DP-1
done

# Launch polybar
$HOME/.config/polybar/launch.sh
