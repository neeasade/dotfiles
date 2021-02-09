#!/bin/sh

PATH=$PATH:$HOME/bin
node_dir=$1
case $node_dir in
  east)  dir=x; sign=-lt; emacs_dir=right ;;
  west)  dir=x; sign=-gt; emacs_dir=left ;;
  north) dir=y; sign=-gt; emacs_dir=up ;;
  south) dir=y; sign=-lt; emacs_dir=down ;;
esac

looking_at=$(jget -r app "$(yabai -m query --windows --window)")

if [ "$looking_at" = "Emacs" ]; then
  if timeout 0.2 elisp "(evil-window-$emacs_dir 1) t"; then
    exit 0
  fi
fi

yabai -m config mouse_follows_focus on
yabai -m window --focus $node_dir
yabai -m config mouse_follows_focus off
