## mpd

My typical scheme is to symlink the music dir to ~/Music and just have my mpd conf work from there. It creates a fifo in /tmp/mpd.fifo which is then used by my ncmpcpp config as well. I use MPDCron to scrobble to last.fm and to trigger an INotification upon song change/status change.
