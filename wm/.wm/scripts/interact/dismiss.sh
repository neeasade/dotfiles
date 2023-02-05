#!/bin/sh
# dismiss what I'm looking at, maybe throw it in the hole

wid=$(bspc query -N -n)
state=$(btags state-plain | grep $wid)

echo "$state"

if [ -z "$state" ]; then
  # it's untagged, send it to the hole
  btags set-tag-on-wids hole $wid
  exit 0
fi

if echo "$state" | grep -q " true "; then
  # it's in a tag, turn it off
  btags toggle "$(echo "$state" | awk '{print $1}')"
else
  # todo: maybe force tag render state here instead
  bspc node $wid -g hidden=true
fi
