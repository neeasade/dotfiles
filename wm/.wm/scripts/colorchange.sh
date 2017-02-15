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
    temptotal=$(echo $section | tr ':' ' '| wc -w)
    [ $temptotal -gt $total ] && total=$temptotal
  done

  IFS=\|
  i=0
  for section in $barInfo; do
    # reverse count on left
    [ "$i" = "0" ] && j=$((total-1)) || j=0

    IFS=':'
    for lemon in $section; do
      eval $lemon=$j
      [ "$i" = "0" ] && j=$((j-1)) || j=$((j+1))
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

# reverse the steps by total
reverseSteps() {
  # by lemon
  lemons=$(echo $barInfo | tr '|:' ' ')
  IFS=' '
  for lemon in $lemons; do
    temp=$(eval "echo \${$lemon}")
    eval $lemon=$((total-temp))
  done
  IFS=
}

# get gradient by step, use total as step.
gradientGet() {
  # 1-indexed, skip the first gradient step as it's the color itself.
  step=$((step+2))
  echo -n "$(gradient $color0 $color7 $total | sed -n ${step}p)"
}


# options
separateStep 
#togetherStep
reverseSteps

color="$1"
step="$(eval "echo \${${2}"})"
ground="$3"

# handle lemon not in barInfo case
if [ -z "$step" ]; then
    echo -n "$1"
    exit
fi

# TODO
if [ "$ground" = "fg" ]; then
    echo -n "$1"
    exit
fi

# do what you want with the steps here..
gradientGet 
