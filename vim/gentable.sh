#!/bin/bash
# generate vim plugin table for vim plug plugins with comment on the end.

key=
value=

echo "plugin | notes"
echo "----|-----"

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
