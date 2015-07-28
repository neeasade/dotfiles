# -*- coding: utf-8 -*-
# Author: lavaramano <lavaramano AT gmail DOT com>
# Improved by: BaSh - <bash.lnx AT gmail DOT com>
# Ported to Weechat 0.3.0 by: Sharn - <sharntehnub AT gmail DOT com)
# This Plugin Calls the libnotify bindings via python when somebody says your nickname, sends you a query, etc.
# To make it work, you may need to download: python-notify2 (and libnotify - libgtk)
# Requires Weechat 0.3.0
# Released under GNU GPL v2
# 2014-05-10, Sébastien Helleu <flashcode@flashtux.org>
#     version 0.0.8: change hook_print callback argument type of
#                    displayed/highlight (WeeChat >= 1.0)
# 2013-11-17, spline <http://github.com/reticulatingspline>
#     version 0.0.7: Fix up prnt statement in previous edition.
#     add in option to not notify on messages starting with prefix like *
#     this is to stop getting notifications if user is connected via ZNC
#     where you command ZNC via irc and all its targets start with '*'.
# 2013-11-16, spline <http://github.com/reticulatingspline>
#     version 0.0.6: Convert from pynotify to notify2 (pynotify is being retired)
#     clean up .show() so it's in a try/except block so we don't get errors in console
#     we also add in an option to disable notifications when away.
#     improve privmsg highlight so if we /msg someone but not in current buffer, it won't notify.
# 2010-02-20, Aron Griffis <agriffis@n01se.net>
#     version 0.0.5: Add nick_separator, don't call show_notification twice on
#     privmsg, fix spelling s/nofify/notify/, use nick as "summary" for privmsg
#     notification, fit in 80 columns, tweak vim modeline.
# 2010-01-24, David Rubin <davidrub+weechat@gmail.com>
#     version 0.0.4.2 Fixed issue with self notifications when used with out "smart_notification"
# 2010-01-19, Didier Roche <didrocks@ubuntu.com>
#     version 0.0.4.1: add private message sender name
# 2010-01-19, Didier Roche <didrocks@ubuntu.com>
#     version 0.0.4: add smart notification:
#     be notified only if you're not in the current channel/pv window (off by default)
# 2009-06-16, kba <unixprog@gmail.com.org>:
#     version 0.0.3: added config options for icon and urgency
# 2009-05-02, FlashCode <flashcode@flashtux.org>:
#     version 0.0.2.1: sync with last API changes

# script variables
SCRIPT_NAME = "notify"
SCRIPT_AUTHOR = "lavaramano"
SCRIPT_VERSION = "0.0.8"
SCRIPT_LICENSE = "GPL"
SCRIPT_DESC = "notify: A real time notification system for weechat"

# make sure we're run under weechat.
import_ok = True
try:
    import weechat
except ImportError:
    print "This script must be run under WeeChat."
    print "Get WeeChat now at: http://www.weechat.org/"
    import_ok = False
# make sure we have notify2.
try:
    import notify2
except ImportError, message:
    print "Missing package(s) for %s: %s" % (SCRIPT_NAME, message)
    print "You must have notify2 installed."
    import_ok = False

# script options
settings = {
    "show_hilights"             : "on",
    "show_priv_msg"             : "on",
    "notify_when_away"          : "off",
    "nick_separator"            : ": ",
    "ignore_nicks_startwith"    : "*",
    "icon"                      : "/usr/share/pixmaps/weechat.xpm",
    "urgency"                   : "normal",
    "smart_notification"        : "off"
}

urgencies = {
    "low"      : notify2.URGENCY_LOW,
    "critical" : notify2.URGENCY_CRITICAL,
    "normal"   : notify2.URGENCY_NORMAL,
}

# Functions
def notify_show(data, bufferp, uber_empty, tagsn, isdisplayed, ishilight, prefix, message):
    """Sends highlighted message to be printed on notification"""
    # string for show_notification return
    snreturn = None
    # smart_notification
    if (weechat.config_get_plugin('smart_notification') == "on" and bufferp == weechat.current_buffer()):
        pass
    # are we away and want highlights? check: w.infolist_integer(infolist, 'is_away')
    elif (weechat.config_get_plugin('notify_when_away') == "off" and weechat.buffer_get_string(bufferp, 'localvar_away')):
        pass
    elif (weechat.buffer_get_string(bufferp, "localvar_type") == "private" and weechat.config_get_plugin('show_priv_msg') == "on"):
        # should we ignore messages from something like ZNC with * prefix?
        ignprefix = weechat.config_get_plugin('ignore_nicks_startwith')
        if ignprefix != '' and prefix.startswith(ignprefix):  # if not empty..
            pass
        # if im sending a message to someone, don't pop up a notification.
        elif weechat.buffer_get_string(bufferp, "localvar_nick") != prefix:
            snreturn = show_notification(prefix, message)
    elif (int(ishilight) and weechat.config_get_plugin('show_hilights') == "on"):
        buffer = (weechat.buffer_get_string(bufferp, "short_name") or weechat.buffer_get_string(bufferp, "name"))
        snreturn = show_notification(buffer, prefix + weechat.config_get_plugin('nick_separator') + message)
    # check to see if we had an error showing notification and return to user
    if snreturn:
        weechat.prnt(bufferp, snreturn)
    return weechat.WEECHAT_RC_OK

def show_notification(chan, message):
    """Our handler to print highlighted messages"""

    notify2.init("weechat")
    wn = notify2.Notification(chan, message, weechat.config_get_plugin('icon'))
    wn.set_urgency(urgencies[weechat.config_get_plugin('urgency')] or notify2.URGENCY_NORMAL)
    # now try to show notification
    try:
        wn.show()
        return None
    except Exception, e:
        return "Exception trying to show notification: {0}".format(e)

if __name__ == "__main__":
    if import_ok and weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE, SCRIPT_DESC, "", ""):
        # Init everything
        for option, default_value in settings.items():
            if weechat.config_get_plugin(option) == "":
                weechat.config_set_plugin(option, default_value)
        # Hook privmsg/hilights
        weechat.hook_print("", "irc_privmsg", "", 1, "notify_show", "")
        # w.hook_info('%s_buffer' %SCRIPT_NAME, '', '', 'info_hook_cb', '')

# vim:set shiftwidth=4 tabstop=4 softtabstop=4 expandtab textwidth=250:
