#!/usr/bin/env dash

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

    # failed, assume edge of monitor, get
    # parent until we are bigger than ourself, then go

    # compare height or width to parent
    self_measure="$(bspc query -T -n "$node" | jq .rectangle.$dim)"
    parent_measure="$(bspc query -T -n "${node}#@parent" | jq .rectangle.$dim)"
    parent_measure="${parent_measure:-$self_measure}" # if there is no parent, this is blank

    if [ "$parent_measure" -gt "$self_measure" ]; then
      # get our sibling
      node_sibling="$(bspc query -N -n "${node}#@brother")"

      compare() {
        us="$(bspc query -T -n "$node" | jq .rectangle.$2)"
        sibling="$(bspc query -T -n "$node_sibling" | jq .rectangle.$2)"
        iif "[ $us -$1 $sibling ]"
      }

      is_top="$(compare lt y)"
      is_left="$(compare lt x)"

      [ "$dim" = "height" ] &&
        bspc node "${node}#@parent" -R "$(iif $is_top $(iif "[ "$dir" = "west" ]" '-90 90' '90 -90'))"

      [ "$dim" = "width" ] &&
        bspc node "${node}#@parent" -R "$(iif $is_left $(iif "[ "$dir" = "north" ]" '90 -90' '-90 90'))"
    else
      # climb the tree, but only allow it to happen once:
      if ! $has_climbed; then
        has_climbed=true
        node="$(bspc query -N -n "${node}#@parent")"
        tiled_move
      fi
    fi
  fi
}


has_climbed=false

if bspc query -N -n $node.floating > /dev/null; then
  floating_move
else
  tiled_move
fi

bspc config pointer_follows_focus false
