#!/usr/bin/env dash
# todo: handle interop between floating and tiled better

dir=$1

bspc config pointer_follows_monitor true
bspc config pointer_follows_focus true
if ! bspc node -f $dir.local; then
    bspc config focus_follows_pointer false
    bspc monitor -f $dir
    bspc config focus_follows_pointer true
fi
bspc config pointer_follows_monitor false
bspc config pointer_follows_focus false
