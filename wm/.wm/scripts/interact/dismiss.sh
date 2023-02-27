#!/bin/sh
# dismiss what I'm looking at, maybe throw it in the hole

wid=$(bspc query -N -n)

read tag active color wids < <(btags state-plain | grep "$wid")

if [ "$tag" = "untagged" ]; then
  # it's untagged, send it to the hole
  btags set-tag-on-wids hole $wid
  exit 0
fi

if $active; then
  btags toggle "$tag"
else
  # todo: maybe force tag render state here instead
  bspc node $wid -g hidden=true
fi
