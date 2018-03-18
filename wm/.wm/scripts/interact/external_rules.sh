#!/usr/bin/env dash
# neeasade
# depends on: jq
# goal: natural feeling window spawning
# behavior:
# if there are any presel's on current desktop, spawn there.
# else, spawn a prefered split in current node based on node size.

# if any one shot rule exists, defer to potentially that.
# have to check a file/set outside of this script because bspc calls here make external rule take priority
wid=$1
class_name="$2"

# preferred split directions:
horiPref=east
vertPref=south

# if dimensions are within this percent, will split $horiPref,
# else direction is determined by width or height being greater.
percent=.33

# if 1 node is open, switch directions (related to custom_monocle)
mon_width=$(bspc query -T -m | jq .rectangle.width)
mon_height=$(bspc query -T -m | jq .rectangle.height)
if [ $mon_width -gt $mon_height ]; then
    node_count=$(bspc query -N -d $desk -n .leaf.normal | wc -l)
    [ $node_count -eq 1 ] && vertPref=$horiPref
fi

# get any presels on the current desktop, select one if so.
presel="$(bspc query -N -d -n .\!automatic | head -n 1)"
targetNode=${presel:-focused}

# we get these values early because you can't use bspc commands
# in an external rule after you start echoing.
width=$(bspc query -T -n $targetNode | jq '.rectangle.width')
height=$(bspc query -T -n $targetNode | jq '.rectangle.height')

# see support_window
[ "$class_name" = "below" ] && echo layer=below

# set node
echo node=$targetNode

# not specifying split dir if targetting presel
[ ! -z $presel ] && exit 0


bcInput="$(iif "[ $width -gt $height ]" \
    "($width-$height)/$width" \
    "($height-$width)/$height" )"

result=$(echo "$bcInput < $percent" | bc -l)

if [ "$result" -eq "1" ]; then
    split_dir=$vertPref
else
    split_dir=$(iif "[ $width -gt $height ]" $horiPref $vertPref)
fi

echo split_dir=$split_dir
