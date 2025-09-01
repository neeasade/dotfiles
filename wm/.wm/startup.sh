#!/bin/sh

. "$HOME/.sh.d/environment"

launch() {
    vdo setsid nohup "$@" &
}

wait_for_internet() {
  tries=10
  while [ $tries -gt 0 ]; do
    if silent ping -c 1 google.com; then
      break
    fi
    sleep 1
    tries=$((tries - 1))
  done
}

if ! lsusb | grep -q Keyboard; then
  systemctl --user disable dunst
  systemctl --user stop dunst
  systemctl --user disable panel
  systemctl --user stop panel
  # launch steam -bigpicture
  gapt false

  bash -ic 'scu-restart unclutter' &

  pegasus-fe
  exit 0
fi

ltheme bg
panelt true

if [ "$(hostname)" = guthix ]; then
  colemak
  xbright 60

  toggle_trackpad
  launch emacs

  bash -ic turn_off_turbo

elif [ "$(hostname)" = bliss ]; then
  launch emacs
  wait_for_internet
  launch "$BROWSER"
fi
