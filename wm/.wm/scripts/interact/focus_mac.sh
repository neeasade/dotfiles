#!/bin/sh

PATH=$PATH:$HOME/bin

node_dir=$1
case $node_dir in
  east)  emacs_dir=right ;;
  west)  emacs_dir=left ;;
  north) emacs_dir=up ;;
  south) emacs_dir=down ;;
esac

try_emacs_dir() {
  if timeout 0.2 elisp "(evil-window-${emacs_dir} 1) t"; then
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
yabai -m window --focus "$node_dir"
yabai -m config mouse_follows_focus off
