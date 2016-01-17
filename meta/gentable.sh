#!/usr/bin/env bash
# generate markdown tables from some conf files.

gen_bspwm()
{
    echo "variable | default | note"
    echo "---------|---------|-----"

    IFS=$'\r\n'
    while read line; do
        if [[ ! -z $line ]]; then
            if echo "$line" | grep -q "=" ; then
            if ! echo "$line" | grep -q "\[" ; then
                line=$(sed 's/export //' <<< "$line")
                name=$(echo "$line" | sed 's/=.*//')
                value=$(echo "$line" | sed 's/[^=]*=//;s/ #.*//')
                if echo "$line" | grep -q " #"; then
                    desc=$(echo "$line" | sed 's/.* #//' | cut -c2-)
                else
                    desc="none"
                fi
                echo "\`$name\` | \`$value\` | $desc"
                name=
                value=
                desc=
            fi
            fi
        fi
    done<"$1"
}

gen_sxhkd()
{
    echo "key | bind"
    echo "----|-----"

    IFS=$'\r\n'
    while read line; do
        if [[ ! -z $line ]]; then
            case "$line" in
                \#*) ;;
                $'    '*)
                    if [[ -z $value ]]; then
                        value='` '"$line"
                    else
                        value="$value"' `<br>` '"$line"
                        value="$(echo "$value" | tr '\r\n' ' ')"
                        value="$(echo "$value" | sed 's/    /\t/')"
                    fi
                    ;;
                *)
                    key="$line"
                    ;;
            esac
        else
            if [[ ! -z $value ]]; then
                # time to print the next row.
                value="$value"' `'
                echo "$key | $value"
                key=
                value=
            fi
        fi
    done<"$1"
}

gen_vim()
{
    echo "plugin | notes"
    echo "-------|-------"

    IFS=$'\r\n'
    while read line; do
        if [[ ! -z $line ]]; then
            case "$line" in
                \#*) ;;
                $'    Plug'*)
                    url="https://github.com/$(echo "$line" | grep -oE "[^']+/[^']+" | head -n 1)"
                    name="$(echo "$line" | grep -oE "[^']+/[^']+" | head -n 1)"
                    desc="$(echo "$line" | grep -oE "\".*" | cut -c2-)"
                    echo "[$name]($url) | $desc "
                    ;;
                *)
                    ;;
            esac
        fi
    done<"$1"
}

key=
value=

for conf in "$*"; do
    case $conf in
        sxhkd) file="../sxhkd/.config/sxhkd/sxhkdrc" ;;
        bspwm) file="../bspwm/.config/bspwm/themes/base.theme"
        vim) file="../vim/.vimrc"
        *) ;;
    esac
done


