#!/bin/sh
# swap between fake fullscreen modes as monocle mode
# this is so we can enjoy fake padding in fullscreen things (and account for panel presence)
# todo: this script assumes that gaps are always what you want.

# todo: figure out skhd environment
. $HOME/.sh.d/environment

# possible modes
do_monocle_padded() {

  # native fullscreen cancels out skhd bindings and generally just acts.. weird.
  # yabai -m window --toggle native-fullscreen

  # yaboi padding $(theme getval x_padding)
  yaboi padding $(theme getval b_window_gap)
  # yaboi padding 0
  # yaboi config window_border off
  yaboi toggle window zoom-fullscreen true
}

do_monocle_slim() {
  mon_width=$(yaboi query display | jq .frame.w)
  percent=$(theme getval b_monocle_window_percent)
  monocle_pad_width=$(bb "(/ (- $mon_width (* 0${percent} ${mon_width})) 2)")

  yaboi config left_padding $monocle_pad_width
  yaboi config right_padding $monocle_pad_width

  yaboi config top_padding $(theme getval b_window_gap)
  yaboi config bottom_padding $(theme getval b_window_gap)
  # yaboi config top_padding 0
  # yaboi config bottom_padding 0

  # yaboi config window_border off
  yaboi toggle window zoom-fullscreen true
}

do_fullscreen() {
  # native fullscreen cancels out skhd bindings and generally just acts.. weird.
  yaboi padding 0
  yaboi toggle window zoom-fullscreen true
  # yabai -m config window_border off
}

do_tiled() {
  yaboi toggle window zoom-fullscreen false
  gap=$(theme getval b_window_gap)
  yaboi padding $gap
  yaboi config window_gap $gap
  yaboi config window_border on
}

state=nop
yaboi toggle window floating false

# current_gap=$(yaboi config window_gap)
current_padding=$(yaboi config left_padding)
zoomed=$(yaboi queryprint window zoom-fullscreen)

if $zoomed && test "$current_padding" -eq "0"; then
  state=fullscreen
elif $zoomed && test "$current_padding" -gt 100; then
  state=monocle_slim
elif $zoomed && test "$current_padding" -gt 0; then
  state=monocle_padded
else
  state=tiled
fi

# rotate
echo before: $state

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


echo after: $state
echo do_$state
do_$state
