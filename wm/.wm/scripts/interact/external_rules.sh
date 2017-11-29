#!/usr/bin/env dash
# neeasade
# depends on: jq
# goal: natural feeling window spawning
# behavior:
# if there are any presel's on current desktop, spawn there.
# else, spawn a prefered split in current node based on node size.

# if any one shot rule exists, defer to potentially that.
# have to check a file/set outside of this script because bspc calls here make external rule take priority
if cat /tmp/bspcrules | grep -q ' ->'; then
    # todo: check class.
    rm -rf /tmp/bspcrules
    exit 0
fi

# preferred split directions:
horiPref=east
vertPref=south

# if dimensions are within this percent, will split $horiPref,
# else direction is determined by width or height being greater.
percent=.33

# if 1 node is open, switch directions (related to custom_monocle)
node_count=$(bspc query -N -d $desk -n .leaf | wc -l)
[ $node_count -eq 1 ] && vertPref=$horiPref

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
[ $width -gt $height ] && bcInput="($width-$height)/$width" || bcInput="($height-$width)/$height"
result=$(echo "$bcInput < $percent" | bc -l)

if [ "$result" -eq "1" ]; then
    split_dir=$vertPref
else
    [ $width -gt $height ] && split_dir=east || split_dir=south
fi

echo split_dir=$split_dir
