#!/usr/bin/env dash

bspc node -p \~$*

# single is buggy rn
exit

# if we only have one, use our border thing
if bspc query -N -n focused.leaf && which chbpresel 2>/dev/null 1>&2; then
    (
        cond=true
        while $cond; do
            presel_win=$(xdotool search --classname "presel_feedback")
            if [ ! -z "$presel_win" ]; then
                cond=false
                compton-trans -w $presel_win 0
            fi
        done
    ) &

    bspc node -p $*

    back="$(colort -t $(bspc config focused_border_color))"

    colort -c $back && \
        forward="$(colort -t  100 $(bspc config focused_border_color))" || \
        forward="$(colort -t -100 $(bspc config focused_border_color))"

    dir="$*"
    case $dir in
        north) dir=1 ;;
        south) dir=3 ;;
        east) dir=4 ;;
        west) dir=2 ;;
    esac

    chbpresel -C $forward -c $back -d $dir $(bspc query -N -n)
else
    bspc node -p $*
fi
