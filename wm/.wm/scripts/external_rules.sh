#!/usr/bin/env dash
# neeasade
# depends on: jq
# goal: natural feeling window spawning
# behavior:
# if there are any presel's on current desktop, spawn there.
# else, spawn a prefered split in current node based on node size.

# get any presels on the current desktop, select one if so.
presel="$(bspc query -N -d -n .\!automatic | head -n 1)"
targetNode=${presel:-focused}

# we get these values early because you can't use bspc commands
# in an external rule after you start echoing.
width=$(bspc query -T -n $targetNode | jq '.rectangle.width')
height=$(bspc query -T -n $targetNode | jq '.rectangle.height')

# set node
echo node=$targetNode

# not specifying split dir if targetting presel
[ ! -z $presel ] && exit 0

# determine split dir on focused node with preferred direction.
[ $width -gt $height ] && split_dir=east || split_dir=south
echo split_dir=$split_dir
