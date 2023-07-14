#!/bin/sh

. $HOME/.sh.d/environment

if [ "$(hostname)" = trouw ]; then
    xbright
fi

if [ "$(hostname)" = bliss ]; then
    # fix screen tearing
    # https://wiki.archlinux.org/title/NVIDIA/Troubleshooting#Avoid_screen_tearing
    nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"

    xrandr --output DP-0 --mode 3840x2160 --rate 120.00

    # don't go to sleep pls
    xset -dpms
    xset s off
fi

wmname LG3D
xsetroot -cursor_name left_ptr
export xstart=$(date +%s)

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &

if ${RUNWM:-true}; then
    exec $HOME/.wm/wm
fi
