#!/bin/sh
# this is called at 5 second intervals (from the name of the file)
# this is the 'work' version of the misc lemon

. "$HOME/.sh.d/environment"

style="| font=Charter | size=12"
style=$(theme -R '| font={{font.panel.family}} | size={{font.panel.size}}')

delim=🫧
delim='🌺'

# want: time to next appt/org ical stuff
# with org-jira, maybe dropdown links to tickets here? or in the jump tbh

check_battery() {
  battery_percent=$(pmset -g batt | awk '-F;' '/%/{print $1}' | awk '{print $NF}' | tr -d '%')
  if [ "$battery_percent" -lt 40 ]; then
    echo "bat: $battery_percent"
  fi
}

check_slack_dms() {
  echo
  # title=$(yaboi query windows | jq -r '.[] | select(.app == "Slack") | .title')
  # if echo "$title" | grep -q 'new item' ; then
  #   echo '🔷Slack🔷'
  # fi
}

result=$(
    (
        check_slack_dms
        check_battery
        cache_output 30 org_task
        cache_output 500 elisp -r '(ns/org-status-outdated)'
        # day_progress

        # echo woo
        # cache_output 30 org_task
    ) | awk NF | tr '\n' '^' | sed 's/.$//' | sed "s/\^/ ${delim} /g")

if [ -z "$result" ]; then
  exit
fi

# printf "- %s - %s\n" "$result" "$style"
printf "${delim} %s ${delim} %s\n" "$result" "$style"
