#!/usr/bin/env bash
# override theme intentions in a generative fashion.
# takes 2 arguments, the lemon and the target.
# target may be one of: bg, fg, activefg, activebg, line, activeline
# theme variables are exposed here to use and reference.

lemon_target="$1"
lemon_reload="$2"

# barInfo example:
# dropdown:desktop|title|clock
barInfo="$p_format"

# vanilla/theme intention for all targets:

vanilla() {
  case $1 in
    fg) echo  "$p_fg_inactive" ;;
    bg) echo  "$p_bg_inactive" ;;
    activefg) echo  "$p_fg_active" ;;
    activebg) echo  "$p_bg_active" ;;
    line) echo  "$p_bg_inactive" ;;
    activeline) echo  "$p_bg_active" ;;
    prefix) echo  "";;
    suffix) echo  "";;
  esac
}

# ignore lemons not included.
if ! echo $barInfo | grep -q $lemon_target; then
  vanilla
  exit
fi

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
  echo "$(gradient $color0 $color7 $total | sed -n ${step}p)"
}


# keep these dynamic unless otherwise,
# because the fg color peek is neat and saves time.
# means we need default bg activebg as well...
if [ ! "$(type -t fg)" = "function" ]; then
  fg() {
    colort -c "$(bg)" && check="$p_bg_normal" || check="$p_fg_normal"
    colort -c "$check" && echo $p_fg_normal || echo $p_bg_normal
  }
fi

if [ ! "$(type -t activefg)" = "function" ]; then
  activefg() {
    colort -c "$(activebg)" && check="$p_bg_active" || check="$p_fg_active"
    colort -c "$check" && echo $p_fg_active || echo $p_bg_active
  }
fi

if [ ! "$(type -t bg)" = "function" ]; then
    bg() {
        vanilla bg
    }
fi

if [ ! "$(type -t activebg)" = "function" ]; then
    activebg() {
        vanilla bg
    }
fi

# Where the magic happens:

if [ ! "$(type -t stepSetup)" = "function" ]; then
    stepSetup() {
        separateStep
        reverseSteps
    }
fi

stepSetup
step="$(eval "echo \${${1}"})"

# declare, reload.
echo "[$lemon_target]"
if [ ! -z "$lemon_reload" ]; then
    echo "reload = \"$lemon_reload\""
fi

# todo: account for mouse generative things here...

# targets may be exported functions by themes in bash.
# resources include: gradientStep, step reference, theme variables.
targets="bg fg line activebg activefg activeline prefix suffix"
IFS=" "
for target in $targets; do
    if type -t $target > /dev/null; then
      #echo "$target = $(eval $target)"
      input="$(eval $target)"
    else
      #echo "$target = \"$(vanilla $target)\""
      input="$(vanilla $target)"
    fi

    if [ ! -z "$input" ]; then
      echo "$target = \"${input}\""
    fi
done

# after that, add any overrides from theme.
if [ ! -z "$(eval echo \$p_${lemon_target}_theme)" ]; then
  eval echo "\$p_${lemon_target}_theme"
fi
