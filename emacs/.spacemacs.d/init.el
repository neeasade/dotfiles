;; -*- mode: emacs-lisp -*-

(load "~/.spacemacs.d/lisp/load-util.el")
(load-from "lisp" "spacemacs")

(defun dotspacemacs/user-config ()
  (load-from "lisp" "config util style")

  ;; see https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bemacs/org#important-note
  (with-eval-after-load 'org
    (load-from "lisp" "org")
    )

  (neeasade/style)
  )
