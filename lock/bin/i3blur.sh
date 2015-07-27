# How to use:
#
# Just place a lock.png in your home folder to overlay whatever you want

#!/bin/bash
scrot 'tmp.png' -e 'convert -blur 0x3 $f ./lockbg.png'
convert -gravity center -composite lockbg.png ~/.lock.png lockfinal.png
i3lock -u -i lockfinal.png
rm lockfinal.png lockbg.png tmp.png
