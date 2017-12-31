;;; init.el --- neeasade
;;; commentary:
;;; code:

(load "~/.emacs.d/lisp/theworld.el")

;; (init-use-package)
(init-straight)

(defun neeasade/core()
  (load "~/.emacs.d/lisp/helpers.el")

  (neeasade/settings-sanity)
  (neeasade/evil)
  (neeasade/interface)
  (neeasade/editing)
  (neeasade/indenting)

  (load "~/.emacs.d/lisp/interactive.el")
  )

(defun neeasade/extra()
  (neeasade/org)
  (neeasade/git)
  (neeasade/company)
  (neeasade/projectile)
  (neeasade/flycheck)
  (neeasade/ivy-style)
  (neeasade/window-management)
  (neeasade/style)
  (neeasade/emms)
  (neeasade/dumbjump)
  (neeasade/treemacs)
  )

(defun neeasade/communication()
  ;; TODO: irc, email, slack.
  (neeasade/irc)
  )

(defun neeasade/development()
  (neeasade/clojure)
  (neeasade/elisp)
  (neeasade/nix)
  (neeasade/javascript)
  )

(defun neeasade/windows()
  ;; TODO: windows-scripts layer from spacemacs
  (neeasade/targetprocess)
  )

(neeasade/core)
(neeasade/extra)
(neeasade/communication)
(neeasade/development)

(provide 'init)

;;; init.el ends here
