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
    reveal_window() {
        wid=$1
        yabai -m window --focus $wid
    }

    while read -r wid; do
  read -r title
  add_switch "window: $title" "reveal_window $wid"
    done < <(yaboi query windows | jq -r '.[] | .id, .title')
}

# this is silly, and intentional.
add_emacss() {
    find_emacs_window() {
        emacs_wid=
        check_id() {
            wid=$1
            if [ ! -z "$emacs_wid" ]; then
                  return 0
            fi

            if yabai -m query --windows --window $wid | jq -r .app | grep Emacs; then
                  emacs_wid=$wid
                  return 0
            fi
            return 1
        }

        check_id $(bspc query -N -n)
        for wid in $(yaboi query windows | jq -r '.[] | .id'); do
      check_id $wid && break
        done

        if [ -z "$emacs_wid" ]; then
              elisp '(ns/spawn-terminal)'
              check_id $(bspc query -N -n)
        fi

        bspc node -f $emacs_wid
    }

    emacs_find_file() {
        find_emacs_window
        elisp "(find-file \"${*}\")"
    }

    emacs_find_buffer() {
        find_emacs_window
        elisp "(-> \"${*}\" get-buffer switch-to-buffer)"
    }

    IFS=$'\n'
    buffers=$(elisp -r '(->> (ns/jump-file-candidates) (mapcar (function s-clean)) (ns/make-lines))')
    for buffer in $buffers; do
  # note: this will break if ever switching to a file with a ' in the name
  add_switch "emacs: $buffer" "emacs_find_file '$buffer'"
    done

    buffers=$(elisp -r "(->> (buffer-list) (-filter (fn (not (buffer-file-name <>)))) (mapcar 'buffer-name) (ns/make-lines))")
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
    add_switch "meta: slack" "qb_meta_open '$(cache_output $((60 60 24)) pass slack/url)'"

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

    echo "target: '$target'"
    echo "match: ${matches_to_actions[${target}]}"

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

if [ ! -z "$SWITCH" ]; then
      type=$(awk -F: '{print $1}' <<< "$SWITCH")
      ASSOCIATE=true enact "$type" >/dev/null
      select_action "$SWITCH"
      exit $?
fi

if [ -z "$SWITCH_KIND" ]; then
      # do_narrow_dmenu
      do_broad_dmenu
else
    $SWITCH_KIND
fi