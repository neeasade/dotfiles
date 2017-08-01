#!/usr/bin/env dash
# todo: handle interop between floating and tiled better

dir=$1

bspc config pointer_follows_monitor true
bspc config pointer_follows_focus true

if bspc query -N -n .focused.local.fullscreen; then
    bspc monitor -f $dir
    exit
fi

if ! bspc node -f $dir.local; then
    bspc config focus_follows_pointer false
    if ! bspc monitor -f $dir; then
      # assume we went up or down and failed, assume monocle, shuffle through style
      [ "$dir" = "north" ] && bspc node -f next.local
      [ "$dir" = "south" ] && bspc node -f prev.local
    fi

    bspc config focus_follows_pointer true
fi

bspc config pointer_follows_monitor false
bspc config pointer_follows_focus false
