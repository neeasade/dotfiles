#!/usr/bin/env bash
# switch between 'things'

. $HOME/.sh.d/environment

set -a

declare -A matches_to_actions

add_switch() {
    if ${ASSOCIATE:-false}; then
          matches_to_actions["$1"]=$2
    fi

    echo "$1"
}

add_windows() {
    while read -r wid title; do
      if [ ! "$title" = "zoom.us" ]; then
          add_switch "window: $title" "yaboi window focus $wid"
      fi
    done < <(yaboi query windows | jq -r '.[] | ((.id|tostring) + " " + .app)')
}

# this is silly, and intentional.
add_emacss() {
    emacs_find_file() {
        find_class emacs || elisp '(ns/spawn-terminal)'
        elisp "(find-file \"${*}\")"
    }

    emacs_find_buffer() {
        find_class emacs || elisp '(ns/spawn-terminal)'
        elisp "(-> \"${*}\" get-buffer switch-to-buffer)"
    }

    IFS=$'\n'
    buffers=$(elisp -r '(ns/make-lines (append (ns/jump-file-candidates) (ns/jump-file-candidates :buffers-without-files)))')
    for buffer in $buffers; do
  # note: this will break if ever switching to a file with a ' in the name
      add_switch "emacs: $buffer" "emacs_find_file '$buffer'"
    done

    buffers=$(elisp -r '(ns/make-lines (ns/jump-file-candidates :buffers-without-files))')
    for buffer in $buffers; do
  add_switch "emacs: $buffer" "emacs_find_buffer '$buffer'"
    done
}

# nb: name intentional.
add_historys() {
    start_day=$(date --date="7 days ago" +%F)
    end_day=$(date +%F)
    start=$(date -d "$start_day 00:00:00" +%s)
    end=$(date -d "$end_day 23:59:59" +%s)
    query="select distinct url, title from history where atime > ${start} and atime < ${end}"
    qutedb="$HOME/.local/share/qutebrowser/history.sqlite"

    while read -r result; do
  url=$(awk -F\| '{print $1}' <<<"$result")
  title=$(awk -F\| '{print $2}' <<<"$result")
  add_switch "history: $title" "$BROWSER '$url'"
  # done < <(sqlite3 "$qutedb" "$query" | head -n 2)
    done < <(sqlite3 "$qutedb" "$query" | head -n 300)
}

add_tabs() {
    open_titles() {
        qb_session_dump | yq -r '.windows[0].tabs[] | .history[-1] | (.url + " " + .title)'
    }

    # user by add_meta as well
    qute_switch_to() {
        title=$*
        qb_meta_open
        qb_command ":tab-select $title"
    }

    while read -r url title; do
  add_switch "tab: $title $url" "qute_switch_to '$(echo "$url" | sed "s/'//g;s/\"// ")'"
    done <<< "$(open_titles)"
}

add_tags() {
    while read -r name; do
  add_switch "tag: set $name" "btags set-tag-on-wids $name \$(bspwindows)"
    done < <(btags state-raw | bb -o '(->> *input* :tags (map :name))')
}

# playing around, ideas:
# - limit tabs to domain -- IE only search for github tabs
# - search against open github issues/prs you have
# - jump to jira stuff (get from org-jira?)
add_metas() {
    # issue here -- slack title changes a lot -- want to just use one title partial match, 'Slack |'
    # add_switch "meta: slack" "qb_meta_open '$(cache_output $((60 * 60 * 24)) pass slack/url)'"
    add_switch "meta: slack" "find_class slack || (nohup slack &)"

    add_switch "meta: linkmarks" "elisp -t 15 '(linkmark-select)'"

    # dmenu_exec() {
    #     save_file="$HOME/.dmenu_exec_history"
    #     choice=$(cat "$save_file" | sort | uniq | dmenu)
    #     echo "$choice" >> "$save_file"
    #     sh -c "$choice"
    # }

    # add_switch "meta: exec" "dmenu_exec"
    # add_switch "meta: org capture" "elisp '(ns/org-capture-popup)'"
}

select_action() {
    # maybe consider allow partial match later (turned out accumulating options was really slow/maybe revisit this in keys)

    if [ -z "$*" ]; then
          echo "no action selected"
          return
    fi

    # target=$(echo "$options" | grep "$*" | head -n 1)
    target=$*

    echo "selected: '$target'"
    echo "action: ${matches_to_actions[${target}]}"

    # echo "${matches_to_actions[${key}]}"
    eval "${matches_to_actions[${target}]}"
    # bspc desktop -l monocle
}

# doing it this way let's us call dmenu right away/just start typing
enact() {
    echo FEEDER_PID=$$

    if [ -z "$*" ]; then
          add_metas
          # add_tags
          add_windows
          add_tabs
          add_emacss
          # add_historys | sort | uniq
    else
        # echo eval add_${*}s
        eval add_${*}s
        add_windows
    fi
}

do_broad_dmenu() {
    choice=$(bash -c "enact | dmenu $@")
    type=$(awk -F: '{print $1}' <<< "$choice")
    ASSOCIATE=true enact "$type" >/dev/null
    select_action "$choice"
}

act_on() {
    SWITCH=$*
    type=$(awk -F: '{print $1}' <<< "$SWITCH")
    ASSOCIATE=true enact "$type" >/dev/null
    select_action "$SWITCH"
    exit $?
}

# pick something ahead of time
if [ -n "$SWITCH" ]; then
    act_on "$SWITCH"
fi

if [ -z "$SWITCH_KIND" ]; then
    do_broad_dmenu
else
    $SWITCH_KIND
fi
