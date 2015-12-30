## bspwm

### Multihead
5 desktops are made per monitor, and super + # will focuse on that desktop for the current monitor. A panel is created per desktop, with workspace status for that monitor. Additionally, the title on a monitor is for the last active window on a desktop, as determined by a change in bspc window focus history. This behavior can be changed in the title.sh script. Windows can be moved across monitors and workspaces in an i3-like fashion, see [this post](http://blog.neeasade.net/2015/04/28/BSPWM-Multihead.html) for more details.

### Tabbing
One feature of this setup is a 'tabbed' display in bar when a desktop is in monocle mode. The active window title is highlighted and the other window titles are clickable on the bar to focus them, though you may also cycle through them with the appropriate key combination. This idea came after a user complained about no i3-like tab mode in bspwm in a thread discussing bspwm's flexibility. Optionally, desktops may always be 'tabbed', resulting lemonbar will show titles of all programs in a desktop view. This is only toggled by watching bspwm's window history, so title mode will only change on a change of window focus, sadly.

### Themes
My WM setup is theme oriented, with the current 'theme file' being symlinked to ~/.bspwm_theme. These are located here in the ~/.config/bspwm/themes folder. All that these theme files do is set environment variables that are used by my bspwmrc, panel scripts, and the ltheme script. Many of the colors are pulled from the current loaded termite config colors. 


variable | default | note
----|----|-
`TERMITE_CONFIG` | `TERMITE_CONFIG=$1` | none
`b_border_width` | `b_border_width=3` | none
`b_window_gap` | `b_window_gap=10` | none
`b_split_ratio` | `b_split_ratio=0.6` | none
`b_normal_border_color` | `b_normal_border_color="#$(getTermColor color0)"` | none
`b_active_border_color` | `b_active_border_color="#$(getTermColor color8)"` | none
`b_focused_border_color` | `b_focused_border_color="#$(getTermColor foreground)"` | none
`b_presel_feedback_color` | `b_presel_feedback_color="#$(getTermColor color8)"` | none
`b_urgent_border_color` | `b_urgent_border_color="#FFFF00"` | none
`b_focused_locked_border_color` | `b_focused_locked_border_color="#FFFFFF"` | none
`PANEL_FIFO` | `PANEL_FIFO=/tmp/panel-fifo` | Location of the Panel fifo
`PANEL_HEIGHT` | `PANEL_HEIGHT=16` | Panel height.
`PANEL_FONT_MAIN` | `PANEL_FONT_MAIN="Dejavu Sans-8"` | Panel main font.
`PANEL_FONT_ICON` | `PANEL_FONT_ICON="FontAwesome-10"` | Panel Icon font.
`PANEL_GAP` | `PANEL_GAP=4` | Gaps of panel off screen.
`PANEL_UNDERLINE` | `PANEL_UNDERLINE=4` | This value is used for both underline and overline
`pPadding` | `pPadding=3` | number of spaces to pad panel informaiton with
`showFreeDesktops` | `showFreeDesktops=true` | option to show nothing for free desktops
`NoModeToggle` | `NoModeToggle=` | if defined, no monocle/tiling toggle.
`pUrgent` | `pUrgent="#ffffff00"` | none
`pActive` | `pActive="#ff$(getTermColor color15)"` | none
`pRest` | `pRest="#ff$(getTermColor color6)"` | none
`pBG` | `pBG="#ff$(getTermColor background)"   ` | Default foreground
`pFG` | `pFG="#ff$(getTermColor foreground)"   ` | Default foreground
`pBGActiveTab` | `pBGActiveTab="$pActive"               ` | Active title tab
`pBGInactiveTab` | `pBGInactiveTab="$pRest"               ` | Inactive title tab - also used to info.sh ouput background.
`pIcon` | `pIcon="#ffaaaaaa"                     ` | color of icons from info.sh
`DO` | `DO=$(echo -e 'uf0c8') ` | occupied desktop
`DF` | `DF=$(echo -e 'uf096') ` | free desktop
`DT` | `DT=$(echo -e 'uf009') ` | tiling mode
`DM` | `DM=$(echo -e 'uf0c9') ` | monocle mode
`# it is possible to specify different information per monitor with barInfo<mon #>` | `# it is possible to specify different information per monitor with barInfo<mon` | ="options"
`barInfo` | `barInfo=""` | Default options/items to include in info section of panel.
`    barInfo2` | `    barInfo2="yaourtUpdates"` | none
`    barInfo1` | `    barInfo1="themeSwitch volume clock"` | none
`    barInfo3` | `    barInfo3="mpd clock"` | none
`DUNST_FONT` | `DUNST_FONT="Dejavu Sans Mono 10"` | Font used by dunst on startup
