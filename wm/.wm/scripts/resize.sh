#!/usr/bin/env dash
# depends on: jq, bc, bspc
# (percent of current monitor resolution to move)
percent=3

# get rectangle property of origin node, floating or tiling (x,y,width,height)
dim()
{
	bspc query -T -n $originalNode | jq ".rectangle.$1"
}

# set fall back, and target window property.
case $1 in
	left)   fallDir=right;  targetProp=width;  queryDir=x; op="head"; sign=-;;
	right)  fallDir=left;   targetProp=width;  queryDir=x; op="tail";;
	top)    fallDir=bottom; targetProp=height; queryDir=y; op="head"; sign=-;;
	bottom) fallDir=top;    targetProp=height; queryDir=y; op="tail";;
	*) exit 1;;
esac

# if we're focused on a group of nodes, select a window within, based on desired direction.
originalNode=$(bspc query -N -n)
afterAction="bspc node -f $originalNode"
if bspc query -N -n focused.\!window; then
	targetNode=$(bspc query -T -n | jq ".. | .?, .root?, .firstChild?, .secondChild? | select (.client != null) | [.] | sort_by(.rectangle.$queryDir) | .[] | .id" | $op -n 1)
	bspc node -f $targetNode
fi

# set move args
moveArgs="$sign$(echo "$percent/100*$(bspc query -T -m | jq .rectangle.$targetProp)" | bc -l)"
[ $targetProp = "height" ] && moveArgs="0 $moveArgs" || moveArgs="$moveArgs 0"

# do the thing
beforeVal=$(dim $targetProp)
bspc node -z $1 $moveArgs

[ "$beforeVal" = "$(dim $targetProp)" ] && bspc node -z $fallDir $moveArgs

if [ "$beforeVal" = "$(dim $targetProp)" ]; then
	# we were focused on a group, do funky things.
	afterAction="$afterAction && bspc node -B"
	case $1 in
		left)   bspc node -f $originalNode; bspc node -f  east;;
		right)  bspc node -f $originalNode; bspc node -f  west;;
		top)    bspc node -f $originalNode; bspc node -f south;;
		bottom) bspc node -f $originalNode; bspc node -f north;;
	esac
	bspc node -z $1 $moveArgs
fi

eval $afterAction
