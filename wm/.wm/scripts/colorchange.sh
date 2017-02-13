#!/bin/bash
# do things with lemon colors

# dropdown:desktop|title|clock
# it's late.
i=0
left=
center=
right=
for section in $(echo $barInfo | tr '|' '\n'); do
    total=$(echo $section | tr ':' '\n' | wc -l)
    [[ "$i" = "0" ]] && j=$total || j=0
    for lemon in $(echo $section | tr ':' '\n'); do
        # reverse count on left
        eval $lemon=$j
        [[ "$i" = "0" ]] && j=$((j-1)) || j=$((j+1))
    done
    i=$((i+1))
done

#example call: colorchange.sh "#ffcccccc" volume fg
# $volume would expect section and index, eg 'right,2'
color="$1"
step="$(eval "echo \${${2}"})"
ground="$3"

# bland/vanilla::
#echo -n "$1"
#exit
if [ -z "$step" ]; then
    echo -n "$1"
    exit
fi

if [ "$ground" = "fg" ]; then
    echo -n "$1"
    exit
fi

colort $(( step * 10 )) "$color"
