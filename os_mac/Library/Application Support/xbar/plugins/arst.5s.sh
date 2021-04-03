#!/bin/sh
# this is called at 5 second intervals
# I don't understand the difference between "refreshing" and "cycling"

. "$HOME/.sh.d/environment"

# want: time to next appt/org ical stuff
# with org-jira, maybe dropdown links to tickets here? or in the jump tbh

check_slack_dms() {
  title=$(yaboi query windows | jq -r '.[] | select(.app == "Slack") | .title')
  if echo "$title" | grep -q \! ; then
    echo Slack
  fi
}

(
    # cache_output 60 org_task
    check_slack_dms

    cache_output 500 elisp -r '(ns/export-scheduled-org-headings-past)'
) | awk NF | tr '\n' '^' | sed 's/.$//' | sed 's/\^/ • /g'

# NB: style is pending: https://github.com/matryer/xbar/issues/630
printf "%5s%s\n" "%" "| font=Charter | size=18"
