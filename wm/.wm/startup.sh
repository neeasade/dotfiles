#!/bin/sh

. "$HOME/.sh.d/environment"

launch() {
    vdo setsid nohup "$*" &
}

panelt
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
