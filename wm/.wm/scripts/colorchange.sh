#!/usr/bin/env dash
# do meta things with lemon colors

# bland/vanilla::
#echo -n "$1"
#exit

# barInfo example:
# dropdown:desktop|title|clock

# step will be per section, total will be largest section.
separateStep() {
  IFS=\|
  total=0
  for section in $barInfo; do
    temptotal=$(echo $barInfo | tr -d ':' | wc -w)
    [ $temptotal -gt $total ] && total=$temptotal
  done

  IFS=\|
  i=0
  for section in $barInfo; do
    # total=$(echo $barInfo | tr -d -C ':' | wc -c)
    # reverse count on left

    if [ "$1" = "desc" ]; then
      [ "$i" = "0" ] && j=$((total-1)) || j=0
    else
      [ "$i" = "0" ] && j=0 || j=$((total-1))
    fi

    IFS=':'
    for lemon in $section; do
      eval $lemon=$j
      if [ "$1" = "desc" ]; then
        [ "$i" = "0" ] && j=$((j-1)) || j=$((j+1))
      else
        [ "$i" = "0" ] && j=$((j+1)) || j=$((j-1))
      fi
    done
    i=$((i+1))
  done
  IFS=
}

# step will be across all sections, total will be number of lemons
togetherStep() {
    total=$(echo $barInfo | tr ':|' ' '| wc -w)

    IFS=\|:
    j=0
    for lemon in $barInfo; do
        eval $lemon=$j
        j=$((j+1))
    done
    IFS=
}

# get gradient by step, use total as step.
gradientGet() {
    # 1-indexed, skip the first gradient step as it's the color itself.
    step=$((step+2))
    echo -n "$(gradient $color0 $color7 $total | sed -n ${step}p)"
}


separateStep 
#togetherStep

color="$1"
step="$(eval "echo \${${2}"})"
ground="$3"

if [ -z "$step" ]; then
    echo -n "$1"
    exit
fi

if [ "$ground" = "fg" ]; then
    echo -n "$1"
    exit
fi

gradientGet 
