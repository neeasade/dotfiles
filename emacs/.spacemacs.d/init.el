;; -*- mode: emacs-lisp -*-

(load "~/.spacemacs.d/lisp/load-util")
(load "~/.spacemacs.d/lisp/spacemacs")

(defun dotspacemacs/user-config ()
  (load "~/.spacemacs.d/lisp/config")
  (load "~/.spacemacs.d/lisp/org")
  (load "~/.spacemacs.d/lisp/util")

  (load "~/.spacemacs.d/lisp/style")
  (neeasade/style)
  )
