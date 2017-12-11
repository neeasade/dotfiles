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
(setq zoom-size '(0.58 . 0.618))
(setq zoom-ignored-buffer-name-regexps '("^*helm" "^helm"))
(zoom-mode t)

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

(evil-vimish-fold-mode 1)

;; testing out
(defun myscroll(count)
  ;; window-total-size gets lines count when called with no args
  ;; note: this only works well for buffers that take more than the full screen...
  (let (
        (windowcount (/ (window-total-size) 2))
        (scrollcount (/ (window-total-size) 6))
        (buffercount (count-lines (point-min) (point-max)))
        )
    (if (> buffercount windowcount)
        (evil-scroll-line-down scrollcount)
        nil
        )
    )
  )

(add-function :after (symbol-function 'evil-scroll-line-to-center) #'myscroll)

(setq alert-default-style 'libnotify)

;; hooks for pomodoro mode pause/play
(defun pomodoro-state-change(message option)
  (alert message)
  (shell-command (concat "player.sh " option))
  )

(add-hook 'org-pomodoro-started-hook
          (apply-partially #'pomodoro-state-change "Pomodoro Started" "play"))

(add-hook 'org-pomodoro-break-finished-hook
          (apply-partially #'pomodoro-state-change "Break over, pick a task!" "play"))

(add-hook 'org-pomodoro-finished-hook
          (apply-partially #'pomodoro-state-change "Pomodoro finished" "pause"))
