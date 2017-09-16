;; -*- mode: emacs-lisp -*-

(load "~/.spacemacs.d/lisp/load-util.el")
(load-from "lisp" "spacemacs")

(defun dotspacemacs/user-config ()
  (load-from "lisp" "config org util style")
  (neeasade/style)
  )
