#!/usr/bin/env dash
# depends on: jq, bc, bspc
# resize windows or groups of windows in bspwm.
# (percent of current monitor resolution to resize)
percent=5
dir=$1

# get rectangle property of origin node, floating or tiling (x,y,width,height)
originalNode=$(bspc query -N -n)
dim() {
	bspc query -T -n $originalNode | jq ".rectangle.$1"
}

# set fall back, and target window property.
case $dir in
	west)   dir=left;   fallDir=right;  targetProp=width;  queryDir=x; op="min"; sign=-;;
	east)  dir=right;  fallDir=left;   targetProp=width;  queryDir=x; op="max"; sign=+;;
	north)    dir=top;    fallDir=bottom; targetProp=height; queryDir=y; op="min"; sign=-;;
	south) dir=bottom; fallDir=top;    targetProp=height; queryDir=y; op="max"; sign=+;;
	*) exit 1;;
esac

# if we're focused on a group of nodes, select a window within, leaning towards our desired direction.
bspc query -N -n focused.\!window > /dev/null && targetNode=$(bspc query -T -n | jq "[recurse(.[]?) | objects | select(has(\"id\") and .client!=null)] | ${op}_by(.rectangle.$queryDir).id")
targetNode=${targetNode:-focused}

# set move args
moveArgs="$sign$(echo "$percent/100*$(bspc query -T -m | jq .rectangle.$targetProp)" | bc -l)"
[ $targetProp = "height" ] && moveArgs="0 $moveArgs" || moveArgs="$moveArgs 0"

# note current state, initial move attempt.
beforeVal=$(dim $targetProp)
bspc node $targetNode -z $dir $moveArgs

# if we're floating, this is all that is needed.
bspc query -N -n focused.floating && exit 0

# if we weren't successful, try resizing the other way
[ "$beforeVal" = "$(dim $targetProp)" ] && bspc node $targetNode -z $fallDir $moveArgs

if [ "$beforeVal" = "$(dim $targetProp)" ]; then
	# undo our wrong resize
	bspc node $targetNode -z $fallDir $(echo $moveArgs | tr +- -+) &

	# attempt to jump the other direction and push into the node
	case $dir in
		left)   targetNode=east;;
		right)  targetNode=west;;
		top)    targetNode=south;;
		bottom) targetNode=north;;
	esac
	bspc node $targetNode -z $dir $moveArgs &
fi
