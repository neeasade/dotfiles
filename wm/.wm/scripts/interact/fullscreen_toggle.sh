#!/bin/sh
# swap between fake fullscreen modes as monocle mode
# this is so we can enjoy fake padding in fullscreen things (and account for panel presence)
# todo: this script assumes that gaps are always what you want.

# possible modes
do_monocle_full() {
  if pgrep picom; then
    hsetroot -solid "$(theme -q color.normal.background)" &
  else
    xsetroot -solid "$(theme -q color.normal.background)" &
  fi

  bspc query -N -n focused.fullscreen && \
    bspc node -t ~fullscreen

  bspc node -t tiled
  bspc desktop -l monocle

  # what are we looking at?
  window_class=$(xprop -id $(bspc query -N -n) | awk -F \" '/WM_CLASS/{print $4}')

  # Wine is for eve
  no_trim_list='.openmw-wrapped
Civ5XP
mpv
Google-chrome
dota2
factorio
Wine'

  if echo "$no_trim_list" | grep "$window_class"; then
    bspc config window_gap 0
  else
    bspc config window_gap $(theme -q x.padding)
  fi

  bspc config left_monocle_padding 0
  bspc config right_monocle_padding 0
  bspc config top_monocle_padding 0
  bspc config bottom_monocle_padding 0

  bspc config borderless_monocle true
}

do_monocle_slim() {
  ltheme bg & >/dev/null

  bspc config borderless_monocle false

  bspc query -N -n focused.fullscreen && bspc node -t ~fullscreen
  bspc node -t tiled

  if pgrep lemonbar; then
    bspc config window_gap $(theme -q bspwm.window-gap)
  else
    bspc config window_gap 0
  fi

  mon_width=$(bspc query -T -m | jq .rectangle.width)
  percent=$(theme -q bspwm.monocle-window-percent)
  window_width=$(echo $percent \* $mon_width | bc -l)

  monocle_pad_width=$(echo "($mon_width - $window_width)/2" | bc -l)
  bspc config left_monocle_padding $monocle_pad_width
  bspc config right_monocle_padding $monocle_pad_width

  for c in {top,bottom}_{,monocle_}padding; do
    echo bspc config $c 0
    bspc config $c 0
  done

  bspc desktop -l monocle
}

do_fullscreen() {
  bspc node -t fullscreen
}

do_tiled() {
  ltheme bg &
  bspc query -N -n focused.fullscreen \
    && bspc node -t ~fullscreen

  # bspc config focused_border_color \#$(theme -q b_focused_border_color)
  bspc desktop -l tiled
  bspc config window_gap $(theme -q bspwm.window.gap)
  bspc config borderless_monocle false

  # bspwm_kill_visual
  # nohup setsid tag_borders &

  # $HOME/.config/bspwm/bspwmrc
}

state=nop

if bspc query -N -n focused.fullscreen; then
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
        state=monocle_full
      elif [ $(bspc config window_gap) -eq $(theme -q x.padding) ]; then
        state=monocle_full
      fi
    fi
  fi
fi

# rotate
echo before: $state

if [ -z "$SLIM" ]; then
  case $state in
    monocle_full) state=tiled ;;
    tiled) state=monocle_full ;;
    monocle_slim) state=monocle_full ;;
    # fullscreen) exit 0;;
    fullscreen) state=tiled ;;
  esac
else
  case $state in
    # monocle_full) state=tiled ;;
    monocle_full) state=monocle_slim ;;
    tiled) state=monocle_slim ;;
    monocle_slim) state=tiled ;;
    # fullscreen) exit 0;;
    fullscreen) state=tiled ;;
  esac
fi

echo after: $state
echo do_$state
do_$state
