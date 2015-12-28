## Weechat

My Weechat is configured to have thick window borders via use of extended ascii characters. Nicks longer than 8 characters get cut off with an elipses. Joins/Leaves are noted with arrows, and mentions are colored differently than regular text. You can see much of this here: <todo>. Colors used are taken from term colors (eg dark gray/yellow/blue) in preference to 'hard coded' colors (eg color 253).

Plugins | use
--------|----
autoconnect.py | Tracks what channels/servers you were connected too when you close the weechat session, and automatically connect to them on start.
buffers.pl | Makes a list of buffers/channels, that can be configured to be indented/numbered.
highmon.pl | Creates a buffer that contains messages containing your nick. can be configured to catch other keywords as well.
autosort.py | Sort your buffers by server in the buffer list.
notify.py | Trigger an INotify message whenever your nick is mentioned (Which is then caught by dunst/whatever)
