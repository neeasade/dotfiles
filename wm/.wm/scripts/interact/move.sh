#!/usr/bin/env dash

bspc config pointer_follows_focus true
dir=$1
node=$(bspc query -N -n)
mon=$(bspc query -M -m)

# are we floating? move a percentage of the monitor resolution.
if bspc query -N -n $node.floating > /dev/null; then
    percent=5
    case $dir in
        west)  targetProp=width; sign=-;;
        east)  targetProp=width;;
        north) targetProp=height; sign=-;;
        south) targetProp=height;;
    esac

    moveArgs="$sign$(echo "$percent/100*$(bspc query -T -m | jq .rectangle.$targetProp)" | bc -l)"
    [ $targetProp = "height"  ] && moveArgs="0 $moveArgs" || moveArgs="$moveArgs 0"
    bspc node -v $moveArgs
else
    # we're tiled. TODO: re-evaluate this
    if ! bspc node -f $dir.local; then
        bspc node $node -m $dir
        bspc monitor -f $dir
    else
        bspc config focus_follows_pointer false
        new_mon=$(bspc query -M -m)
        [ "$new_mon" = "$mon" ] &&
            bspc node -s $node  ||
            bspc node $node -m $new_mon
        bspc config focus_follows_pointer true
    fi
    bspc node -f $node
fi

bspc config pointer_follows_focus false
