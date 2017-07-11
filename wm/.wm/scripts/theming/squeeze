#!/usr/bin/env bash
# override theme intentions in a generative fashion.
# takes 2 arguments, the lemon and the target.
# target may be one of: bg, fg, activefg, activebg, line, activeline
# theme variables are exposed here to use and reference.
# prereq: colorsetup.sh

lemon_target="$1"
step="$(eval "echo \${${1}}")"
lemon_target_align="$(eval "echo \${${1}_align}")"

# ignore lemons not included.
if ! echo $barInfo | grep -q $lemon_target; then
    vanilla
    exit
fi

# declare, reload.
echo "[$lemon_target]"
if [ ! -z "$lemon_reload" ]; then
    echo "reload = \"$lemon_reload\""
fi

# todo: account for mouse generative things here

# targets may be exported functions by themes in bash.
# resources include: gradientStep, step reference, theme variables.
targets="bg fg line"

if grep -q "meta active" < "$HOME/.wm/panel/lemons/$lemon_target"; then
    targets="$targets activebg activefg activeline"
fi

targets="$targets prefix suffix"

IFS=" "
for target in $targets; do
    if type -t $target > /dev/null; then
      input="$(eval $target)"
    else
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

# things that aren't colors may be provided by p_{lemon_target}_{option}
options="reload mouse_left mouse_right mouse_middle scroll_up scroll_down overline underline"
IFS=' '
for option in $options; do
    if [ ! -z "$(eval echo "\$p_${lemon_target}_${option}")" ]; then
        echo $(tr '_' '-' <<< $option) = \"$(eval echo "\$p_${lemon_target}_${option}")\"
    fi
done
IFS=

echo ""
