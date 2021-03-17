#!/bin/sh
# this is called at 5 second intervals
# I don't understand the difference between "refreshing" and "cycling"

. "$HOME/.sh.d/environment"

# want: time to next appt/org ical stuff
# with org-jira, maybe dropdown links to tickets here? or in the jump tbh

# idea: check title qutebrowser session for slack dm

(
    org_task

    cache_output 500 elisp -r '(ns/export-scheduled-org-headings-past)'
) | awk NF | tr '\n' '^' | sed 's/.$//' | sed 's/\^/ • /g'

# NB: style is pending: https://github.com/matryer/xbar/issues/630
printf "%5s%s\n" "%" "| font=Charter | size=16"
