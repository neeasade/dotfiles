#!/bin/sh

. "$HOME/.sh.d/environment"

launch() {
    vdo setsid nohup "$@" &
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

panelt true
ltheme bg
launch emacs

# wait for internet
tries=10
while [ $tries -gt 0 ]; do
  if silent ping -c 1 google.com; then
    launch "$BROWSER"
    break
  fi
  sleep 1
  tries=$((tries - 1))
done

# launch $TERMINAL
# colemak

