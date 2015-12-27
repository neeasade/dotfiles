#!/bin/bash

# generate a github markdown table from an sxhkd table 

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
					value='``` sh '"$line"
				else
					value="$value"'```<br>``` sh'"$line"
				fi
				;;
			*)
				key="$line"
				;;
		esac
	else
		if [[ ! -z $value ]]; then
			# time to print the next row.
			value="$value"'```<br>'
			echo "$key | $value"
			key=
			value=
		fi
	fi

done<"$1"
