#!/bin/sh
# this is called at 5 second intervals (from the name of the file)

. "$HOME/.sh.d/environment"
style="| font=Charter | size=18"

# want: time to next appt/org ical stuff
# with org-jira, maybe dropdown links to tickets here? or in the jump tbh

check_slack_dms() {
  title=$(yaboi query windows | jq -r '.[] | select(.app == "Slack") | .title')
  if echo "$title" | grep -q \! ; then
    echo '🌻Slack🌻'
  fi
}

result=$(
    (
        check_slack_dms

        cache_output 30 org_task
        cache_output 500 elisp -r '(ns/export-scheduled-org-headings-past)'
    ) | awk NF | tr '\n' '^' | sed 's/.$//' | sed 's/\^/ 🌳 /g')

if [ -z "$result" ]; then
  echo
else
  printf "🌳🌳 %s 🌳🌳 %s\n" "$result" "$style"
fi
