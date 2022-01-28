#!/usr/bin/env sh
# neeasade
# window switcher dmenu

switch=""
titles=""
wids="$(bspc query -N -n .leaf.\!hidden)"

for wid in $wids; do
  title="$(xtitle $wid)"
  titles="${titles}"$'\n'"${title}"
  switch="$switch '$(echo "${title}" | sed "s#'#\'\\\'\'#g")') bspc node -f $wid;;"
done

switch="$switch *) ;;"

selection="$(echo "$titles" | dmenu "$@")"
eval "case \"$selection\" in $switch esac"
