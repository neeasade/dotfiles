#!/bin/sh
# get dimensions for a floating dialog, maybe act on it.

print_env() {
  dim() {
    # bspc query -T -m | jq .rectangle.$1
    jget "$1" "$(bspc query -T -m)"
  }

  LINES=30
  W=$(echo .66 \* $(dim width) | bc | sed 's/\..*//')
  H=$(echo .33 \* $(dim width) | bc | sed 's/\..*//')
  X=$(( ($(dim width) - W) / 2 ))
  Y=$(( ($(dim height) - H) / 4 ))

  # W=$(echo .20 \* $(dim width) | bc | sed 's/\..*//')
  # H=$(echo .40 \* $(dim height) | bc | sed 's/\..*//')
  # X=$(( ($(dim width) - W) / 2 ))
  # Y=$(( ($(dim height) - H) / 4 ))
  # Y=$((Y+100))

  if [ "$(hostname)" = "geloof" ]; then
    LINES=20
  fi

  echo "W=$W"
  echo "H=$H"
  echo "X=$X"
  echo "Y=$Y"
  echo "LINES=$LINES"
}

set_rule() {
  print_env >/dev/null
  # bspc rule -a \* -o state=floating rectangle=${W}x${H}+${X}+${Y} layer=above
  bspc rule -a \* -o state=floating rectangle=${W}x${H}+${X}+${Y}
}

act_now() {
  print_env >/dev/null

  if [ -z "$wid" ]; then
    wid=$(bspc query -N -n)
  fi

  bspc node $wid -g hidden=off
  bspc node $wid -t floating
  xdotool windowmove $wid $X $Y
  xdotool windowsize $wid $W $H
  bspc node $wid -f
}

while getopts w:ren flag; do
  case $flag in
    w) wid=$OPTARG;;
    r) set_rule;;
    e) print_env;;
    n) act_now;;
    *) echo "options: -r -w <wid> -e -n (rule wid env now) -- wid defaults to the focused node for now" >&2; exit 1;;
  esac
done

