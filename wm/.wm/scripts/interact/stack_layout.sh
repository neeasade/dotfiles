
#!/usr/bin/env bash
# a stack layout for bspwm
# bspc subscribe node_{remove,add} | while read _; do ./stack_layout.sh; done

master_size=.63

jget() {
    # thanks camille
    key=$1
    shift
    var=${*#*\"$key\":}
    var=${var%%[,\}]*}
    echo "$var"
}

vdo() {
    echo "$*"
    "$@"
}

# ensure the count of the master child is 1, or make it so
win_count=$(bspc query -N '@/1' -n .descendant_of.window | wc -l)
echo win_count: $win_count
if [ $win_count -ne 1 ]; then
    if [ -z "$*" ]; then
	new_master=$(bspc query -N '@/1' -n last.descendant_of.window | head -n 1)
	# new_master=$(bspc query -N '@/1' -n .descendant_of.window | head -n 1)
    else
	new_master=$*
    fi

    if [ -z "$new_master" ]; then
	new_master=$(bspc query -N '@/2' -n last.descendant_of.window | head -n 1)
    fi

    echo "new master: $new_master"
    # move everything into 2 that is not our new_master
    for wid in $(bspc query -N '@/1' -n .descendant_of.window | grep -v $new_master); do
	vdo bspc node "$wid" -n '@/2'
    done

    vdo bspc node "$new_master" -n '@/1'
fi

# amend the split type so we are arranged correctly
# on all stacking children
correct_rotation() {
    # sleep 5
    # sleep 0.5
    node=$1
    want=$2
    have=$(jget splitType "$(bspc query -T -n "$node")")
    # the only bashism
    have=${have:1:${#have}-2}

    if [ ! "$have" = "$want" ]; then
	vdo bspc node "$node" -R 270
    fi
}


vdo correct_rotation '@/' vertical
vdo correct_rotation '@/2' horizontal

stack_node=$(bspc query -N '@/2' -n)
for parent in $(bspc query -N '@/2' -n '.descendant_of.!window' | grep -v $stack_node); do
    vdo correct_rotation $parent horizontal
done

stack_node=$(bspc query -N '@/2' -n)
bspc node '@/2' -B

# mon_width=$(bspc query -T -m | jq .rectangle.width)
mon_width=$(jget width "$(bspc query -T -m)")

want=$(echo $master_size \* $mon_width | bc -l | sed 's/\..*//')
have=$(jget width "$(bspc query -T -n '@/1')")
bspc node '@/1' --resize right $((want - have)) 0
