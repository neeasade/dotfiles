#!/bin/sh

PATH=$PATH:$HOME/bin
dir=$1

try_emacs_dir() {
  if timeout 0.2 elisp "(evil-window-${dir} 1) t"; then
    exit 0
  fi
}

if ! looking_at=$(yaboi query window); then
  # we failed to see what we were looking at. assume it's a fullscreen emacs window
  looking_at=Emacs
else
  looking_at=$(jget -r app "$looking_at")
fi

case $looking_at in
  Emacs) try_emacs_dir ;;
  .kitty-wrapped) try_emacs_dir ;;
  kitty) try_emacs_dir ;;
esac

yabai -m config mouse_follows_focus on
yabai -m window --focus "$dir"
yabai -m config mouse_follows_focus off
