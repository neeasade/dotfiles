#!/bin/sh
# swap between fake fullscreen modes as monocle mode
# this is so we can enjoy fake padding in fullscreen things
# this doesn't account for floating or overlapped situations very well

# todo: make this a little smarter? the real issue you are trying to solve here is you like the
# padding when 'fullscreen' in qutebrowser or emacs but not for netflix or mpv

# possible modes
do_monocle_padded() {
  hsetroot -solid "#$(theme getval background)" &
  bspc query -N -n focused.fullscreen && \
    bspc node -t ~fullscreen

  # the idea is that fake padding comes from
  bspc config window_gap $(theme getval x_padding) &
  bspc config left_monocle_padding 0
  bspc config right_monocle_padding 0
  bspc config borderless_monocle true

  bspc desktop -l monocle
}

do_monocle_slim() {
  theme refresh bg &
  bspc query -N -n focused.fullscreen && bspc node -t ~fullscreen

  # this issue is for this width you want softer borders, not borderless
  bspc config borderless_monocle false

  mon_width=$(bspc query -T -m | jq .rectangle.width)
  percent=$(theme getval b_monocle_window_percent)
  window_width=$(echo $percent \* $mon_width | bc -l)
  monocle_pad_width=$(echo "($mon_width - $window_width)/2" | bc -l)
  bspc config left_monocle_padding $monocle_pad_width
  bspc config right_monocle_padding $monocle_pad_width

  bspc desktop -l monocle
}

do_fullscreen() {
  bspc node -t fullscreen
}

do_tiled() {
  theme refresh bg &
  bspc query -N -n focused.fullscreen \
    && bspc node -t ~fullscreen

  # bspc config window_gap $(theme getval b_window_gap)
  # bspc config focused_border_color \#$(theme getval b_focused_border_color)
  bspc desktop -l tiled
  # $HOME/.config/bspwm/bspwmrc
}

state=nop

if bspc query -N -n focused.fullscreen; then
  state=fullscreen
else
  state=tiled

  if [ "$(bspc query -T -d | jq -r .layout)" = "monocle" ]; then
    if [ $(bspc config left_monocle_padding) -gt 0 ]; then
      state=monocle_slim
    else
      state=monocle_padded
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
    fullscreen) exit 0;;
  esac
else
  case $state in
    # monocle_padded) state=tiled ;;
    monocle_padded) state=monocle_slim ;;
    tiled) state=monocle_slim ;;
    monocle_slim) state=tiled ;;
    fullscreen) exit 0;;
  esac
fi


echo after: $state
echo do_$state
do_$state
