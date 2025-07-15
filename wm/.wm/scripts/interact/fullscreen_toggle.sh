#!/usr/bin/env bash
# this is sort of a layout script
# modes: tiled, monocle, monocle_slim, fullscreen

# todo: if mashing super + t and one window is open, tell us there's only one window

gapped=$(iif '[ $(bspc config window_gap) -gt 0 ]')

handle_bd() {
  # new thinking: floating at the side
  # should be a little column of floaters
  # if ! bspc query -N -n '.floating.!hidden'; then
  #   return;
  # fi

  if ! xprop WM_CLASS -id "$(bspc query -N -n '.floating.!hidden' | head -n 1)" | grep -q discord; then
    return;
  fi

  # p=$(bspc config left_padding)
  # bspc config left_padding "$(( $(bspc config left_padding) + 450))"

  read X Y W H <<< "20 100 390 1000"

  # bspc config left_padding "$(( $(bspc config left_padding) + 450))"

  if $gapped; then
    bspc config left_padding "$((W))"
  else
    bspc config left_padding "$((W + X + 40))"
  fi

  # this should act for all windows oopsie
  if [ "$(bspc query -T -n 'any.floating.!hidden' | jq .client.floatingRectangle.width)" -gt $W ]; then
    # notify-send  -u low "hit!"

    wid=$(bspc query -N -n '.floating.!hidden' | head -n 1)
    xdotool windowmove $wid $X $Y
    xdotool windowsize $wid $W $H
  fi
}

do_monocle_full() {
  bspc query -N -n focused.fullscreen && \
    bspc node -t ~fullscreen

  # bspc config left_monocle_padding 0
  # bspc config right_monocle_padding 0
  # bspc desktop -l monocle

  if [ "$before" = "monocle_slim" ]; then
    handle_bd
  fi

  bspc config left_monocle_padding 0
  bspc config right_monocle_padding 0
  bspc desktop -l monocle
}

do_monocle_slim() {
  if [ "$before" = "monocle_full" ]; then
    ltheme bg &
  fi

  # ltheme bg &

  (
    looking_at=$(xprop -id "$(bspc query -N -n)" WM_CLASS | awk -F'"' '{print $4}')
    case "$looking_at" in
      Emacs*) elisp '(delete-other-windows)' ;;
    esac
  ) &

  bspc query -N -n focused.fullscreen && bspc node -t ~fullscreen
  bspc node -t tiled

  monocle_pad_width=$(theme -q bspwm-meta.monocle-pad-width)
  bspc config left_monocle_padding $monocle_pad_width
  bspc config right_monocle_padding $monocle_pad_width

  bspc config borderless_monocle false
  bspc desktop -l monocle
  gapt $gapped
}

do_fullscreen() {
  bspc node -t fullscreen
}

do_tiled() {
  bspc query -N -n focused.fullscreen \
    && bspc node -t ~fullscreen

  handle_bd
  bspc config left_monocle_padding 0
  bspc config right_monocle_padding 0
  gapt $gapped
  bspc desktop -l tiled
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
    if [ $(bspc config right_monocle_padding) -gt 0 ]; then
      state=monocle_slim
    else
      if [ "$(bspc config right_monocle_padding)" = 0 ]; then
        state=monocle_full
      fi
    fi
  fi
fi

echo before: $state
before=$state

if [ ! -z "$*" ]; then
  "$@"
  exit $?
fi

# rotate

if [ -z "$SLIM" ]; then
  case $state in
    monocle_full) state=tiled ;;

    tiled) state=monocle_full ;;
    monocle_slim) state=monocle_full ;;

    # tiled) state=fullscreen ;;
    # monocle_slim) state=fullscreen ;;

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
