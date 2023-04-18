#!/bin/sh

gapped=$(iif '[ $(bspc config window_gap) -gt 0 ]')

do_monocle_full() {
  bspc query -N -n focused.fullscreen && \
    bspc node -t ~fullscreen

  # what are we looking at?
  window_class=$(xprop -id $(bspc query -N -n) | awk -F \" '/WM_CLASS/{print $4}')

  # Wine is for eve
  no_trim_list='.openmw-wrapped
Civ5XP
mpv
steam_app_242920
Google-chrome
dota2
factorio
Wine'

  bspc config left_monocle_padding 0
  bspc config right_monocle_padding 0
  bspc desktop -l monocle

  if echo "$no_trim_list" | grep -q "$window_class"; then
    gapt false 0
  else
    gapt $gapped
  fi
}

do_monocle_slim() {
  ltheme bg &

  bspc query -N -n focused.fullscreen && bspc node -t ~fullscreen
  bspc node -t tiled

  monocle_pad_width=$(theme -q bspwm-meta.monocle-pad-width)

  bspc config left_monocle_padding $monocle_pad_width
  bspc config right_monocle_padding $monocle_pad_width

  bspc config borderless_monocle false
  bspc desktop -l monocle
  gapt $gapped


  looking_at=$(xprop -id "$(bspc query -N -n)" WM_CLASS | awk -F'"' '{print $4}')
  case "$looking_at" in
    Emacs*) elisp '(delete-other-windows)' ;;
  esac
}

do_fullscreen() {
  bspc node -t fullscreen
}

do_tiled() {
  bspc query -N -n focused.fullscreen \
    && bspc node -t ~fullscreen

  bspc desktop -l tiled
  gapt $gapped
}

do_same() {
  do_$state
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
      if [ "$(bspc config left_monocle_padding)" = 0 ]; then
        state=monocle_full
      fi
    fi
  fi
fi

if [ ! -z "$*" ]; then
  "$@"
  exit $?
fi

# rotate
echo before: $state

if [ -z "$SLIM" ]; then
  case $state in
    monocle_full) state=tiled ;;
    tiled) state=monocle_full ;;
    monocle_slim) state=monocle_full ;;
    fullscreen) state=tiled ;;
  esac
else
  case $state in
    # monocle_full) state=tiled ;;
    monocle_full) state=monocle_slim ;;
    tiled) state=monocle_slim ;;
    monocle_slim) state=tiled ;;
    fullscreen) state=tiled ;;
  esac
fi

echo after: $state
echo do_$state
do_$state
