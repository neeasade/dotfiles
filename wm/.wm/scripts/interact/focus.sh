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
  # capture window corners, abcd clockwise from top left
  winmap["${wid}_a_x"]=$x
  winmap["${wid}_a_y"]=$y
  winmap["${wid}_b_x"]=$((x+w))
  winmap["${wid}_b_y"]=$y
  winmap["${wid}_c_x"]=$((x+w))
  winmap["${wid}_c_y"]=$((y + h))
  winmap["${wid}_d_x"]=$x
  winmap["${wid}_d_y"]=$((y+h))
done < <(wattr ixywh $floating_windows $node $og_target)

inside_node() {
  x=$1
  y=$2
  ax="${winmap["${node}_a_x"]}"
  ay="${winmap["${node}_a_y"]}"
  bx="${winmap["${node}_b_x"]}"
  by="${winmap["${node}_b_y"]}"
  dx="${winmap["${node}_d_x"]}"
  dy="${winmap["${node}_d_y"]}"

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
  # if check_corner a b c d; then
  #   return 0;
  # fi

  case $node_dir in
    east) corners="a d";;
    west) corners="b c";;
    north) corners="c d";;
    south) corners="a b";;
  esac

  if check_corner $corners; then
    target_corners $corners
  fi

  if check_corner a b c d; then
    target_corners $corners
  fi

  # make a check-corner for the left or right half of node, allow matching one

}

target_corners() {
  for c in $*; do
    x=${winmap["${wid}_${c}_x"]}
    y=${winmap["${wid}_${c}_y"]}

    if [ -z "$target_x" ]; then
      target_x=$x
      target_y=$y
      target="$wid"
    fi

    # bias towards close corners
    val=${winmap["${wid}_${c}_${dir}"]}
    if [ "$val" "$sign" "$(eval echo \$target_$dir)" ]; then
      target=$wid
      target_x=$x
      target_y=$y
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
