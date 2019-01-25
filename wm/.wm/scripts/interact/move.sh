#!/bin/sh

dir=$1
node=$(bspc query -N -n)

case $dir in
  west)  fdim=width; tdim=height; sign=-;;
  east)  fdim=width; tdim=height;;
  north) fdim=height; tdim=width; sign=-;;
  south) fdim=height; tdim=width;;
esac

floating_move() {
  percent=5
  moveArgs="$sign$(echo "$percent/100*$(bspc query -T -m | jq .rectangle.$fdim)" | bc -l)"
  [ $fdim = "height" ] && moveArgs="0 $moveArgs" || moveArgs="$moveArgs 0"
  bspc node -v $moveArgs
}

tiled_move() {
  (bspc node -n $dir.\!automatic || bspc node -s "$dir") && return

  # compare height or width to parent
  self_measure="$(bspc query -T -n "$node" | jq .rectangle.$tdim)"
  parent_measure="$(bspc query -T -n "${node}#@parent" | jq .rectangle.$tdim)"

  if [ "$parent_measure" -gt "$self_measure" ]; then
    bspc node "${node}#@parent" -p $dir
    bspc node "${node}#@parent" -i

    receptacle_id="$(bspc query -N "${node}#@parent#@parent" -n '.descendant_of.leaf.!window')"
    bspc node $node -n $receptacle_id
    bspc node "${node}#@parent" -B
  else
    node="$(bspc query -N -n "${node}#@parent")"
    [ ! -z "$node" ] && tiled_move
  fi
}

bspc config pointer_follows_focus true
$(bspc query -T -n | jq -r .client.state)_move
bspc node -f
bspc config pointer_follows_focus false
