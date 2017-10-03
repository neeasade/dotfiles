;; -*- mode: emacs-lisp -*-
;; ref: http://orgmode.org/worg/org-configs/org-customization-guide.html
;; inspo: https://github.com/gjstein/emacs.d/blob/master/config/gs-org.el
;;        http://sachachua.com/notebook/emacs/org-config.el
;;        https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/config/local/org-config/org-config.el
;;        https://github.com/glynnforrest/emacs.d/blob/master/site-lisp/setup-org.el

(require 'org-habit)

(load-settings "org"
'(
  ;; where
  directory "~/org/projects"
  agenda-files (list org-directory)
  default-notes-file  "~/org/inbox.org"
  default-diary-file  "~/org/diary.org"
  default-habits-file  "~/org/habits.org"

  ;; style
  bullets-bullet-list '("@" "%" ">" ">")
  ellipsis "…"
  startup-indented t
  startup-folded t

  ;; behavior
  ;; todo-keywords '((sequence "TODO" "NEXT" "WAITING" "INACTIVE" "CANCELLED" "MEETING" "DONE"))
  todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
    (sequence "WAITING(w@/!)" "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)" "MEETING"))

  blank-before-new-entry '((heading . t) (plainlist-item . nil))
  tag-alist '(
             ("test" . ?t)
             ("endtest" . ?e)
             )

  ;; clock
  clock-x11idle-program-name "x11idle"
  clock-idle-time 10
  clock-sound nil
  pomodoro-play-sounds nil
  pomodoro-keep-killed-pomodoro-time t
  pomodoro-ask-upon-killing nil

  ;; capture
  capture-templates
  '(("t" "todo" entry (file org-default-notes-file)
     "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)

    ("b" "Blank" entry (file org-default-notes-file)
     "* %?\n%u")

    ("m" "Meeting" entry (file org-default-notes-file)
     "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)

    ("d" "Diary" entry (file+datetree org-default-diary-file)
     "* %?\n%U\n" :clock-in t :clock-resume t)

    ("D" "Daily Log" entry (file "~/org/daily-log.org")
     "* %u %?\n*Summary*: \n\n*Problem*: \n\n*Insight*: \n\n*Tomorrow*: " :clock-in t :clock-resume t)

    ("i" "Idea" entry (file org-default-notes-file)
     "* %? :IDEA: \n%u" :clock-in t :clock-resume t)

    ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
     "** NEXT %? \nDEADLINE: %t")
    )

  ;; current file or any of the agenda-files, max 9 levels deep
  refile-targets '(
                 (nil :maxlevel . 9)
                 (org-agenda-files :maxlevel . 9)
                 )

  ;; habits (TODO: learn)
  modules '(org-habit)
  habit-show-habits-only-for-today t
  ))
