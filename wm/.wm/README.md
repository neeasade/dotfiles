## bspwm

My bspwmrc file references many variables set by the bspwm theme files for options.

### Multihead
5 desktops are made per monitor, and super + # will focuse on that desktop for the current monitor. A panel is created per desktop, with workspace status for that monitor. Additionally, the title on a monitor is for the last active window on a desktop, as determined by a change in bspc window focus history. This behavior can be changed in the title.sh script. Windows can be moved across monitors and workspaces in an i3-like fashion, see [this post](http://blog.neeasade.net/2015/04/28/BSPWM-Multihead.html) for more details.

### scripts
the scripts folder has various scripts for toggling the panel, gaps, pulsing window borders, and so on.


### Panel
My panel is a mesh of shell scripts that have evolved over time to accomedate flexible features/options as I thought of them/saw fit. The only attractive thing about it is it runs posix shell and has flexible (to me) options. The main panel is lemonbar with dzen2 dropdowns, colors for which are all derived from the currently loaded bspwm theme file. The initial scripts used for info.sh came from z3bra's blog post about bar scripts.

#### Tabbing
One feature of this setup is a 'tabbed' display in bar when a desktop is in monocle mode. The active window title is highlighted and the other window titles are clickable on the bar to focus them, though you may also cycle through them with the appropriate key combination. This idea came after a user complained about no i3-like tab mode in bspwm in a thread discussing bspwm's flexibility. Optionally, desktops may always be 'tabbed', resulting lemonbar will show titles of all programs in a desktop view. This is only toggled by watching bspwm's window history, so title mode will only change on a change of window focus, sadly.

### Themes
My WM setup is theme oriented, with the current 'theme file' being symlinked to ~/.bspwm_theme. These are located here in the ~/.config/bspwm/themes folder. All that these theme files do is set environment variables that are used by my bspwmrc, panel scripts, and the ltheme script. Many of the colors are pulled from the current loaded termite config colors.

variable | default | note
----|----|-----
`TERMITE_CONFIG` | `$1` | none
`b_border_width` | `3` | none
`b_window_gap` | `10` | none
`b_split_ratio` | `0.6` | none
`b_normal_border_color` | `"#$(getTermColor color0)"` | none
`b_active_border_color` | `"#$(getTermColor color8)"` | none
`b_focused_border_color` | `"#$(getTermColor foreground)"` | none
`b_presel_feedback_color` | `"#$(getTermColor color8)"` | none
`b_urgent_border_color` | `"#FFFF00"` | none
`b_focused_locked_border_color` | `"#FFFFFF"` | none
`PANEL_FIFO` | `/tmp/panel-fifo` | Location of the Panel fifo
`PANEL_HEIGHT` | `16` | Panel height.
`PANEL_FONT_MAIN` | `"Dejavu Sans-8"` | Panel main font.
`PANEL_FONT_ICON` | `"FontAwesome-10"` | Panel Icon font.
`PANEL_GAP` | `4` | Gaps of panel off screen.
`PANEL_UNDERLINE` | `4` | This value is used for both underline and overline
`pPadding` | `3` | number of spaces to pad panel informaiton with
`showFreeDesktops` | `true` | option to show nothing for free desktops
`NoModeToggle` | `` | if defined, no monocle/tiling toggle.
`pUrgent` | `"#ffffff00"` | none
`pActive` | `"#ff$(getTermColor color15)"` | none
`pRest` | `"#ff$(getTermColor color6)"` | none
`pBG` | `"#ff$(getTermColor background)"   ` | Default foreground
`pFG` | `"#ff$(getTermColor foreground)"   ` | Default foreground
`pBGActiveTab` | `"$pActive"               ` | Active title tab
`pBGInactiveTab` | `"$pRest"               ` | Inactive title tab - also used to info.sh ouput background.
`pIcon` | `"#ffaaaaaa"                     ` | color of icons from info.sh
`DO` | `$(echo -e 'uf0c8') ` | occupied desktop
`DF` | `$(echo -e 'uf096') ` | free desktop
`DT` | `$(echo -e 'uf009') ` | tiling mode
`DM` | `$(echo -e 'uf0c9') ` | monocle mode
`barInfo` | `""` | Default options/items to include in info section of panel.
`    barInfo2` | `"yaourtUpdates"` | none
`    barInfo1` | `"themeSwitch volume clock"` | none
`    barInfo3` | `"mpd clock"` | none
`DUNST_FONT` | `"Dejavu Sans Mono 10"` | Font used by dunst on startup
