#!/bin/sh
# enact window modes

# todo future settings:
# getopt
# get the current state
# set a desired state

# rotate state:
#   optionally set trim mode rotation

# macos native fullscreen cancels out skhd bindings and generally just acts.. weird -- prefer zoom-fullscreen everywhere.

. $HOME/.sh.d/environment

# possible modes
do_monocle_padded() {
  # yaboi config window_border off
  yaboi padding $(theme getval x_padding)
  yaboi toggle window zoom-fullscreen true
}

do_monocle_slim() {
  mon_width=$(yaboi query display | jq .frame.w)
  percent=$(theme getval b_monocle_window_percent)
  monocle_pad_width=$(bb "(int (Math/floor (/ (- $mon_width (* 0${percent} ${mon_width})) 2)))")


  # yaboi config window_border off
  # yabai -m config active_window_border_color   0xff$(theme getval background)
  yaboi toggle window zoom-fullscreen true
  yabai -m space --padding abs:0:0:${monocle_pad_width}:${monocle_pad_width}
}

do_fullscreen() {
  yaboi toggle window zoom-fullscreen true
  yaboi padding 0
  # yabai -m config window_border off
}

do_tiled() {
  yaboi toggle window zoom-fullscreen false
  gap=$(theme getval b_window_gap)
  yaboi padding $gap
  yaboi config window_gap $gap
  # yaboi config window_border on
}

state=nop
yaboi toggle window floating false

# there isn't a way to get display gap? only global gap, which might be different
# current_padding=$(yaboi config left_padding)

zoomed=$(yaboi query window zoom-fullscreen)

# current_padding used this way is only valid when zoomed
mon_width=$(yaboi query display | jq .frame.w)
win_width=$(yaboi query window | jq .frame.w)
current_padding=$((mon_width - win_width))

if $zoomed && test "$current_padding" -eq "0"; then
  state=fullscreen
elif $zoomed && test "$current_padding" -gt 100; then
  state=monocle_slim
elif $zoomed && test "$current_padding" -gt 0; then
  state=monocle_padded
else
  state=tiled
fi

before=$state

if [ -z "$SLIM" ]; then
  case $state in
    monocle_padded) state=tiled ;;
    tiled) state=monocle_padded ;;
    monocle_slim) state=monocle_padded ;;
    # fullscreen) exit 0;;
    fullscreen) state=tiled ;;
  esac
else
  case $state in
    # monocle_padded) state=tiled ;;
    monocle_padded) state=monocle_slim ;;
    tiled) state=monocle_slim ;;
    monocle_slim) state=tiled ;;
    # fullscreen) exit 0;;
    fullscreen) state=tiled ;;
  esac
fi

echo "state: $before -> $state"
echo do_$state
do_$state

# Wherever you go, there you are.
yaboi window focus $(yaboi query window id)
