#!/bin/bash

# generate a github markdown table from an sxhkd conf

key=
value=

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
