#!/usr/bin/env bash
# enact window modes

# this script is soooo sloooowwwww here, yabai query time is killing us

# todo future settings:
# getopt
# get the current state
# set a desired state

# rotate state:
#   optionally set trim mode rotation

# macos native fullscreen cancels out skhd bindings and generally just acts.. weird
# prefer zoom-fullscreen everywhere.

. $HOME/.sh.d/environment

# possible modes
do_monocle_padded() {
  # future: do the borderless thing
  # yaboi config window_border off
  # yaboi padding $(theme -q x.padding)

  yaboi padding $(theme -q bspwm.window-gap)
  yaboi toggle window zoom-fullscreen true
}

do_monocle_slim() {
  percent=$(theme -q bspwm.monocle-window-percent)
  monocle_pad_width=$(bb "(int (Math/floor (/ (- $mon_width (* 0${percent} ${mon_width})) 2)))")

  # yaboi config window_border off
  # yabai -m config active_window_border_color   0xff$(theme -q background)
  gap=$(theme -q bspwm.window-gap)
  yaboi toggle window zoom-fullscreen true
  echo yabai -m space --padding abs:$gap:$gap:${monocle_pad_width}:${monocle_pad_width}
  yabai -m space --padding abs:$gap:$gap:${monocle_pad_width}:${monocle_pad_width}
}

do_fullscreen() {
  yaboi toggle window zoom-fullscreen true
  yaboi padding 0
  # yabai -m config window_border off
}

do_tiled() {
  yaboi toggle window zoom-fullscreen false
  gap=$(theme -q bspwm.window-gap)
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
mon_width=$(yaboi query display frame w)

if $zoomed; then
  win_width=$(yaboi query window frame w)
  current_padding=$((mon_width - win_width))

  if test "$current_padding" -eq "0"; then
    state=fullscreen
  elif test "$current_padding" -gt 100; then
    state=monocle_slim
  elif test "$current_padding" -gt 0; then
    state=monocle_padded
  fi
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
