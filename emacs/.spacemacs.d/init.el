;; -*- mode: emacs-lisp -*-

; used in others.
(defun load-from (dir targets)
  (dolist (target (split-string targets " "))
    (load (concat "~/.spacemacs.d/" dir "/" target))
    )
  )

(load-from "lisp" "load-util spacemacs")

(defun dotspacemacs/user-config ()
  (load-from "lisp" "config org util style")
  (neeasade/style)
  )
