#!/usr/bin/env dash
# depends on: jq, bc, bspc
# (percent of current monitor resolution to move)
percent=3

# get rectangle property of current floating or tiling node (x,y,width,height)
dim()
{
	bspc query -T -n | jq ".client.$(bspc query -T -n | jq -r .client.state)Rectangle.$1"
}

# set fall back, and target window property.
case $1 in
	left)   fallDir=right;  targetProp=width;  sign=-;;
	right)  fallDir=left;   targetProp=width;;
	top)    fallDir=bottom; targetProp=height; sign=-;;
	bottom) fallDir=top;    targetProp=height;;
	*) exit 1;;
esac

# set move args
moveArgs="$sign$(echo "$percent/100*$(bspc query -T -m | jq .rectangle.$targetProp)" | bc -l)"
[ $targetProp = "height"  ] && moveArgs="0 $moveArgs" || moveArgs="$moveArgs 0"

# do the thing
beforeVal=$(dim $targetProp)
bspc node -z $1 $moveArgs
[ "$beforeVal" = "$(dim $targetProp)"  ] && bspc node -z $fallDir $moveArgs
