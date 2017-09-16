;; -*- mode: emacs-lisp -*-

;; auto accept changes made to file if not changed in current buffer.
(global-auto-revert-mode t)

;; auto-follow symlinks when editing
(setq vc-follow-symlinks t)

;; git
(setq magit-repository-directories "~/git")

;; other
(editorconfig-mode 1)

; auto refresh latex compile doc
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

; give emms some decent auto-evil binds
(add-hook 'emms-browser-show-display-hook 'evil-initialize)

; add bindings for printing elisp eval
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  ; inline
  "ei" 'le::eval-and-insert-results
  ; buffer
  "eb" 'le::eval-and-insert-all-sexps
  )

;(setq emms-browser-default-format "")
