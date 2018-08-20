#!/bin/sh

bspc config pointer_follows_focus true
dir=$1
node=$(bspc query -N -n)


floating_move() {
  case $dir in
    west)  dim=width; sign=-;;
    east)  dim=width;;
    north) dim=height; sign=-;;
    south) dim=height;;
  esac

  percent=5
  moveArgs="$sign$(echo "$percent/100*$(bspc query -T -m | jq .rectangle.$dim)" | bc -l)"
  [ $dim = "height"  ] && moveArgs="0 $moveArgs" || moveArgs="$moveArgs 0"
  bspc node -v $moveArgs
}

tiled_move() {
  if bspc node -f $dir.local; then
    bspc node -s $node
    bspc node -f $node
  else
    case $dir in
      west)  dim=height;;
      east)  dim=height;;
      north) dim=width;;
      south) dim=width;;
    esac

    # compare height or width to parent
    self_measure="$(bspc query -T -n "$node" | jq .rectangle.$dim)"
    parent_measure="$(bspc query -T -n "${node}#@parent" | jq .rectangle.$dim)"
    parent_measure="${parent_measure:-$self_measure}"

    if [ "$parent_measure" -gt "$self_measure" ]; then
      parent="$(bspc query -N -n "${node}#@parent")"
      bspc node $parent -p $dir
      bspc node $parent -i

      # the .leaf.!window query didn't work
      receptacle_id="$(bspc query -T -n "${node}#@parent#@parent" | jq '.. | .?, .root?, .firstChild?, .secondChild? | select (.client == null and .firstChild == null and .secondChild == null) | .id' | grep -v null | head -n 1)"

      bspc node $node -n $receptacle_id
    else
      parent="$(bspc query -N -n "${node}#@parent")"
      if [ ! -z "$parent" ]; then
        node="$parent"
        tiled_move
      fi
    fi
  fi
}

if bspc query -N -n $node.floating > /dev/null; then
  floating_move
else
  tiled_move
fi

bspc config pointer_follows_focus false
