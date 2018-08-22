#!/usr/bin/env bash
dir=$1

node=$(bspc query -N -n)
node=${node^^}
target=$(bspc query -N -n $dir)

# if there are any floating windows, use an edge of that if it overlaps into us
node_dir=$1
case $node_dir in
  east)  dir=x; sign=-lt;;
  west)  dir=x; sign=-gt;;
  north) dir=y; sign=-gt;;
  south) dir=y; sign=-lt;;
esac

floating_windows=$(bspc query -N -d -n .window.local.floating)
declare -A winmap=();
while read wid x y w h; do
  wid="${wid^^}"
  # capture window corners, 1234 clockwise from top left
  winmap["${wid}_1_x"]=$x
  winmap["${wid}_1_y"]=$y
  winmap["${wid}_2_x"]=$((x+w))
  winmap["${wid}_2_y"]=$y
  winmap["${wid}_3_x"]=$((x+w))
  winmap["${wid}_3_y"]=$((y + h))
  winmap["${wid}_4_x"]=$x
  winmap["${wid}_4_y"]=$((y+h))
done < <(wattr ixywh $floating_windows $node $og_target)

inside_node() {
  x=$1
  y=$2
  ax="${winmap["${node}_1_x"]}"
  ay="${winmap["${node}_1_y"]}"
  bx="${winmap["${node}_2_x"]}"
  by="${winmap["${node}_2_y"]}"
  dx="${winmap["${node}_4_x"]}"
  dy="${winmap["${node}_4_y"]}"

  bax=$((bx - ax))
  bay=$((by - ay))
  dax=$((dx - ax))
  day=$((dy - ay))

  [ $(( (x - ax) * bax + (y - ay) * bay )) -lt "0" ] && return 1
  [ $(( (x - bx) * bax + (y - by) * bay )) -gt "0" ] && return 1
  [ $(( (x - ax) * dax + (y - ay) * day )) -lt "0" ] && return 1
  [ $(( (x - dx) * dax + (y - dy) * day )) -gt "0" ] && return 1

  return 0;
}

check_corner() {
  for c in $*; do
    x=${winmap["${wid}_${c}_x"]}
    y=${winmap["${wid}_${c}_y"]}
    if ! inside_node $x $y; then
      return 1
    fi
  done

  return 0
}

check_corners() {
  wid="$1"
  # if they are all inside node, we don't care
  # turns out this meant we couldn't grab floaters in certain situations
  # if check_corner 1 2 3 4; then
  #   return 0;
  # fi

  case $node_dir in
    east) corners="1 4";;
    west) corners="2 3";;
    north) corners="3 4";;
    south) corners="1 2";;
  esac

  # if ! check_corner $corners; then
  #   return
  # fi

  for corner in $corners; do
    x=${winmap["${wid}_${corner}_x"]}
    y=${winmap["${wid}_${corner}_y"]}

    if inside_node $x $y; then
      if [ -z "$target_x" ]; then
        target_x=$x
        target_y=$y
        target="$wid"
      fi

      # bias towards close corners
      val=${winmap["${wid}_${corner}_${dir}"]}
      if [ "$val" "$sign" "$(eval echo \$target_$dir)" ]; then
        target=$wid
        target_x=$x
        target_y=$y
      fi
    fi
  done
}

# if any corner falls in us, use that, biased to higher values
if [ ! -z "$floating_windows" ]; then
  for wid in $floating_windows; do
    wid="${wid^^}"
    [ "$wid" = "$node" ] && continue
    check_corners "$wid"
  done
fi

bspc config pointer_follows_focus true
bspc node -f $target
bspc config pointer_follows_focus false
