#!/bin/env bash

# generate a markdown table from a theme file.

echo "variable | default | note"
echo "----|----|-----"

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
