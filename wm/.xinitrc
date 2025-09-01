#!/bin/sh

. $HOME/.sh.d/environment

if [ "$(hostname)" = bliss ]; then
    # fix screen tearing
    # https://wiki.archlinux.org/title/NVIDIA/Troubleshooting#Avoid_screen_tearing
    nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"

    if xrandr | grep -q 'DP-0 connected'; then
	xrandr --output DP-0 --mode 3840x2160 --rate 120.00

	# eric monitor
	# xrandr --output HDMI-0 --mode 1920x1080 --rate 120.00
    fi

    # to prevent sleep/monitor shut off:
    # xset -dpms
    # xset s off
fi

wmname LG3D
xsetroot -cursor_name left_ptr
export xstart=$(date +%s)

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &

if ${RUNWM:-true}; then
    exec $HOME/.wm/wm
fi
