#!/usr/bin/env dash
# depends on: jq, bc, bspc
# (percent of screen res to move)
percent=3

# get rectangle property of current floating or tiling node (x,y,width,height)
dim()
{
	bspc query -T -n | jq ".client.$(bspc query -T -n | jq -r .client.state)Rectangle.$1"
}

# ehhhhhhh
dimValues()
{
	[ $targetProp = "height" ] && return="0 "
	return="$return$sign$(echo "$percent/100*$(bspc query -T -m | jq .rectangle.$targetProp)" | bc -l)"
	[ $targetProp = "width" ] && return="$return 0"
	echo $return
}

case $1 in
	left)   fallDir=right; targetProp=width; sign=-;;
	right)  fallDir=left;  targetProp=width;;
	top)    fallDir=bottom; sign=-;;
	bottom) fallDir=top;;
esac

targetProp=${targetProp:-height}
before=$(dim $targetProp)
bspc node -z $1 $(dimValues)
[ "$before" = "$(dim $targetProp)" ] && bspc node -z $fallDir $(dimValues)
