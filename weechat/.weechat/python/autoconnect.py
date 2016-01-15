# -*- coding: utf-8 -*-
#
# Copyright (C) 2011 Arnaud Renevier <arno@renevier.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

SCRIPT_NAME    = "autoconnect"
SCRIPT_AUTHOR  = "arno <arno@renevier.net>"
SCRIPT_VERSION = "0.3.0"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "reopens servers and channels opened last time weechat closed"
SCRIPT_COMMAND = "autoconnect"

try:
    import weechat
except:
    print "This script must be run under WeeChat."
    print "Get WeeChat now at: http://www.weechat.org/"
    quit()


weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE, SCRIPT_DESC, "", "")

quitting = False


def get_autojoin_channels(server):
    channels = weechat.config_string(weechat.config_get("irc.server.%s.autojoin" % (server,)))
    return set(channels.split(',')) if channels else set()


def update_autojoin_channels(server, channels):
    channels = ','.join(channels) if channels else "null"
    weechat.command("", "/mute /set irc.server.%s.autojoin %s" % (server, channels))


def joinpart_cb(data, signal, signal_data):
    server = signal.split(',')[0]
    if weechat.info_get("irc_nick_from_host", signal_data) != weechat.info_get("irc_nick", server):
        # nick which has joined is not our current nick
        return weechat.WEECHAT_RC_OK

    autojoin_channels = get_autojoin_channels(server)

    if signal.endswith("irc_in2_JOIN"):
        weechat.command("", "/mute /set irc.server.%s.autoconnect on" % (server,))

        channel = signal_data.split()[-1][1:]
        # Fix up prefixless channels, : prefixed channels
        channel = channel if channel[0] != ':' else channel[1:]
        channel = '#' + channel if channel[0] != '#' else channel
        autojoin_channels.add(channel)

    elif signal.endswith("irc_in2_PART"):
        channel = signal_data.split(' PART ')[1].split()[0]
        autojoin_channels.discard(channel)

    update_autojoin_channels(server, autojoin_channels)
    weechat.command("", "/save irc")
    return weechat.WEECHAT_RC_OK


def disconnect_cb(data, signal, signal_data):
    global quitting
    if not quitting:
        server = signal_data.split(',')[0]

        weechat.command("", "/mute /set irc.server.%s.autoconnect null" % (server,))
        weechat.command("", "/mute /set irc.server.%s.autojoin null" % (server,))

        weechat.command("", "/mute /save irc")
    return weechat.WEECHAT_RC_OK


def quit_cb(data, signal, signal_data):
    global quitting
    quitting = True
    return weechat.WEECHAT_RC_OK


weechat.hook_signal("quit", "quit_cb", "")
weechat.hook_signal("*,irc_in2_join", "joinpart_cb", "")
weechat.hook_signal("*,irc_in2_part", "joinpart_cb", "")
weechat.hook_signal("irc_server_disconnected", "disconnect_cb", "")
