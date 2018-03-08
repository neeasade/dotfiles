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
  (neeasade/shell)
  (neeasade/eshell)

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
  (neeasade/emms)
  (neeasade/jump)
  (neeasade/treemacs)
  (neeasade/target-process)
  (neeasade/dashdocs)
  )

(defun neeasade/communication()
  ;; TODO: irc, email, slack.
  (neeasade/irc)
  (neeasade/slack)
  (neeasade/twitter)
  (neeasade/email)
  )

(defun neeasade/development()
  (neeasade/clojure)
  (neeasade/csharp)
  (neeasade/elisp)
  (neeasade/nix)
  (neeasade/javascript)
  (neeasade/typescript)
  (neeasade/terraform)
  )

(defun neeasade/windows()
  ;; TODO: windows-scripts layer from spacemacs
  )

(neeasade/core)
(neeasade/extra)
(neeasade/communication)
(neeasade/development)

(neeasade/style)
(provide 'init)

;;; init.el ends here
