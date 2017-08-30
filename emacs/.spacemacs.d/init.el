;; -*- mode: emacs-lisp -*-

(defun load-from-dots (targets)
  (dolist (target (split-string targets " "))
    (load (concat "~/.spacemacs.d/lisp/" target))
    )
  )

(load-from-dots "load-util spacemacs")

(defun dotspacemacs/user-config ()
  (load-from-dots "config org util style")
  (neeasade/style)
  )
