;; -*- mode: emacs-lisp -*-

;; see https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bemacs/org#important-note
(with-eval-after-load 'org
  (load-from "lisp" "org")
  )

;; auto accept changes made to file if not changed in current buffer.
(global-auto-revert-mode t)

;; auto-follow symlinks when editing
(setq vc-follow-symlinks t)

;; git
(setq magit-repository-directories (list "~/git"))

;; other
(editorconfig-mode 1)

; zoom testing
;(setq zoom-size '(0.58 . 0.618))
;(setq zoom-ignored-buffer-name-regexps '("^*helm" "^helm"))
;(zoom-mode nil)

;; auto refresh latex compile doc
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; add bindings for printing elisp eval
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  ;; inline
  "ei" 'le::eval-and-insert-results
  ;; buffer
  "eb" 'le::eval-and-insert-all-sexps
  )

;; ensure we don't use outdated elc files
(setq load-prefer-newer t)

;; if eww is displayed, use that, else open here.
(defun eww-browse-existing-or-new (url)
    (if (get-buffer-window "*eww*" 0)
          (url-retrieve url 'eww-render
                        (list url nil (get-buffer "*eww*")))
        (eww url)
      )
 )

(setq helm-dash-browser-func 'eww-browse-existing-or-new)
