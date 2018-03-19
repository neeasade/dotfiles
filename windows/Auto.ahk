#SingleInstance force
; this is on a pok3r 
; notes: to switch esc --> `~ and fn+esc -> esc, do:
; layer switch:
; fn + r_control (prog mode)
; esc
; fn + esc
; pn (finish bind)
; fn + esc
; esc
; pn
; fn + r_control (exit)

Capslock::Ctrl
; maybe
; CapsLock & h:: Send, {Left}
; CapsLock & j:: Send, {Down}
; CapsLock & k:: Send, {Up}
; CapsLock & l:: Send, {Right}

; swap these around to match hhkb layout.
\::Backspace
Backspace::\

+BS::
 KeyWait, Shift
  Send, {|}
Return

; swap left win/alt (there is no win on the right side, pok3r)
LWin::LAlt
LAlt::LWin

; other
; ^SPACE::  Winset, Alwaysontop, , A
