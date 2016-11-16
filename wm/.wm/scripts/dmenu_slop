#!/usr/bin/env bash
# neeasade
# depends: dmenu slop xdo bspc

# get program name to launch using dmenu cache:
cachedir=${XDG_CACHE_HOME:-"$HOME/.cache"}
[ -d "$cachedir" ] && cache=$cachedir/dmenu_run || cache=$HOME/.dmenu_cache

program=$(
	IFS=:
	if stest -dqr -n "$cache" $PATH; then
		stest -flx $PATH | sort -u | tee "$cache" | eval dmenu "$@"
	else
		eval dmenu "$@" < "$cache"
	fi
)

eval `slop`

# Get current window
win=`bspc query -N -n`
bspc rule -a \* -o state=floating

$program &

# Wait for window to spawn and move with xdo.
newwin=$win
while [ "$newwin" = "$win" ]; do
	newwin=`bspc query -N -n`
done

xdo move -x $X -y $Y $newwin &
xdo resize -w $W -h $H $newwin &
