#!/usr/bin/env bash

PATH=$PATH:$HOME/bin

node_dir=$1
case $node_dir in
  east)  dir=x; sign=-lt; emacs_dir=right ;;
  west)  dir=x; sign=-gt; emacs_dir=left ;;
  north) dir=y; sign=-gt; emacs_dir=up ;;
  south) dir=y; sign=-lt; emacs_dir=down ;;
esac

if ! looking_at=$(yaboi query window); then
  # we failed to see what we were looking at. assume it's a fullscreen emacs window
  looking_at=Emacs
else
  looking_at=$(jq -r .app <<< "$looking_at")
fi

try_emacs_dir() {
  if timeout 0.2 elisp "(evil-window-${emacs_dir} 1) t"; then
    exit 0
  fi
}

case $looking_at in
  Emacs) try_emacs_dir ;;
  .kitty-wrapped) try_emacs_dir ;;
  kitty) try_emacs_dir ;;
esac

yabai -m config mouse_follows_focus on
yabai -m window --focus $node_dir
yabai -m config mouse_follows_focus off
