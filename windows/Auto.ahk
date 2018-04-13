#SingleInstance force

Capslock::Ctrl
; todo: consider
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
