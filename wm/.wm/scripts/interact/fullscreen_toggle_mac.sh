#!/bin/sh
# swap between fake fullscreen modes as monocle mode
# this is so we can enjoy fake padding in fullscreen things (and account for panel presence)
# todo: this script assumes that gaps are always what you want.

# possible modes
do_monocle_padded() {

  # native fullscreen cancels out skhd bindings and generally just acts.. weird.
  # yabai -m window --toggle native-fullscreen

  pad=10
  yabai -m config top_padding $pad
  yabai -m config bottom_padding $pad
  yabai -m config left_padding $pad
  yabai -m config right_padding $pad

  # are we
  # if ! yabai-query window zoom-fullscreen; then
  # fi
  yabai -m window --toggle zoom-fullscreen

  # todo later: coordinate bg color and border color
  yabai -m config window_border_width 1

  # need a way to set the the desktop bg programmatically

  bspc query -N -n focused.fullscreen && \
    bspc node -t ~fullscreen

  yabai -m config top_padding 0
  yabai -m config bottom_padding 0

  yabai -m window --toggle zoom-fullscreen
}

do_monocle_slim() {
  # bspc query -N -n focused.fullscreen && bspc node -t ~fullscreen
  # bspc node -t tiled

  mon_width=$(yabai -m query --displays --display | jq .frame.w)
  percent=$(theme getval b_monocle_window_percent)
  window_width=$(echo $percent \* $mon_width | bc -l)
  monocle_pad_width=$(echo "($mon_width - $window_width)/2" | bc -l)

  yabai -m config left_padding $monocle_pad_width
  yabai -m config right_padding $monocle_pad_width

  yabai -m config top_padding 0
  yabai -m config bottom_padding 0

  # 0 is not an option
  # todo later: coordinate bg color and border color
  yabai -m config window_border_width 1

  yabai -m window --toggle zoom-fullscreen
}

do_fullscreen() {
  # native fullscreen cancels out skhd bindings and generally just acts.. weird.
  # yabai -m window --toggle native-fullscreen

  yabai -m config top_padding 0
  yabai -m config bottom_padding 0
  yabai -m config left_padding 0
  yabai -m config right_padding 0
  yabai -m window --toggle zoom-fullscreen

  # todo later: coordinate bg color and border color
  yabai -m config window_border_width 1

}

do_tiled() {

  bspc query -N -n focused.fullscreen \
    && bspc node -t ~fullscreen

  # bspc config focused_border_color \#$(theme getval b_focused_border_color)
  # bspc desktop -l tiled
  # bspc config window_gap $(theme getval b_window_gap)
  # bspc config borderless_monocle false

  gap=$(theme getval b_window_gap)
  yabai -m config top_padding                  $gap
  yabai -m config bottom_padding               $gap
  yabai -m config left_padding                 $gap
  yabai -m config right_padding                $gap
  yabai -m config window_gap                   $gap

  # bspwm_kill_visual
  # nohup setsid tag_borders &

  # $HOME/.config/bspwm/bspwmrc
}

state=nop

current_gap=$(yabai -m config window_gap)
current_padding=$(yabai -m config top_padding)

if yabai-query window zoom-fullscreen && [ "$current_gap" = "0"]; then
  state=fullscreen
else
  state=tiled

  # ensure that we are not floating with these states
  bspc node -t tiled

  if [ "$(bspc query -T -d | jq -r .layout)" = "monocle" ]; then
    if [ $(bspc config left_monocle_padding) -gt 0 ]; then
      state=monocle_slim
    else
      if [ $(bspc config window_gap) -eq 0 ]; then
        state=monocle_padded
      elif [ $(bspc config window_gap) -eq $(theme getval x_padding) ]; then
        state=monocle_padded
      fi
    fi
  fi
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
