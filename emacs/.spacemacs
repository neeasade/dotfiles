;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; split a list into 2, every other element.
(defun split-list (lst)
  (if lst
      (if (cddr lst)
          (let ((l (split-list (cddr lst))))
            (list
             (cons (car lst) (car l))
             (cons (cadr lst) (cadr l))))
        `((,(car lst)) ,(cdr lst)))
    '(nil nil)))

;; load dotspacemacs settings from a list
(defun load-spacemacs-settings(lst)
  (mapcar*
   (lambda (key value)
     (set-default
      (intern (concat "dotspacemacs-" (prin1-to-string key)))
      (eval value)
      )
     )
   (car (split-list lst))
   (car (cdr (split-list lst)))
   )
  )

(defun dotspacemacs/layers ()
  (load-spacemacs-settings '(
      distribution 'spacemacs
      enable-lazy-installation 'unused
      ask-for-lazy-installation t
      configuration-layer-path '()

      configuration-layers '(
        ; languages
        c-c++ clojure
        emacs-lisp
        html
        javascript
        markdown
        nixos
        rust
        (typescript :variables
                    typescript-fmt-on-save t)
        (shell :variables
                shell-default-height 30
                shell-default-position 'bottom)

        ; interface
        better-defaults helm ranger

        ; features
        (auto-completion  :variables
                          auto-completion-tab-key-behavior 'complete
                          auto-completion-complete-with-key-sequence-delay 0
                          )

        colors syntax-checking spell-checking

        ; misc
        yaml
        themes-megapack
        evil-commentary
        git version-control
        org
        )

      additional-packages '( base16-theme )
      frozen-packages '()
      excluded-packages '()
      install-packages 'used-only
    ))
  )

(defun dotspacemacs/init ()
  (load-spacemacs-settings '(
      startup-banner nil
      elpa-https t
      line-numbers 'relative
      elpa-timeout 5
      check-for-update nil
      elpa-subdirectory nil
      editing-style 'hybrid
      verbose-loading nil
      startup-lists '((recents . 5)
                      (projects . 7))
      startup-buffer-responsive t
      scratch-mode 'text-mode
      colorize-cursor-according-to-state t
      themes (list (intern (replace-regexp-in-string "\n$" "" (shell-command-to-string "xrq 'Emacs.theme'"))))
      default-font (list (replace-regexp-in-string "\n$" "" (shell-command-to-string "xrq 'Emacs.font'"))
                          :size 12
                          :weight normal
                          :width normal
                          :powerline-scale 1.4)
      leader-key "SPC"
      emacs-command-key "SPC"
      ex-command-key ":"
      emacs-leader-key "M-m"
      major-mode-leader-key ","
      major-mode-emacs-leader-key "C-M-m"
      distinguish-gui-tab nil
      remap-Y-to-y$ t
      retain-visual-state-on-shift t
      visual-line-move-text nil
      ex-substitute-global nil
      default-layout-name "Default"
      display-default-layout nil
      auto-resume-layouts t
      large-file-size 1
      auto-save-file-location 'cache
      max-rollback-slots 5
      helm-resize nil
      helm-no-header t
      helm-position 'top
      helm-use-fuzzy 'always
      enable-paste-transient-state nil
      which-key-delay 0.4
      which-key-position 'right-then-bottom
      loading-progress-bar t
      fullscreen-at-startup nil
      fullscreen-use-non-native nil
      maximized-at-startup nil
      active-transparency 90
      inactive-transparency 90
      show-transient-state-title t
      show-transient-state-color-guide t
      mode-line-unicode-symbols nil
      smooth-scrolling t
      folding-method 'evil
      smartparens-strict-mode nil
      smart-closing-parenthesis t
      highlight-delimiters 'all
      persistent-server nil
      search-tools '("ag" "pt" "ack" "grep")
      default-package-repository nil
      whitespace-cleanup 'trailing
    ))
  )

(defun dotspacemacs/user-init ()
  ;; todo here: make the file if it doesn't exist
  (setq custom-file (file-truename (concat dotspacemacs-directory ".spacemacs-custom")))
  (load custom-file)
  )

(defun dotspacemacs/user-config ()
  (add-hook 'before-save-hook 'tide-format-before-save)

  ;; disable bold fonts
  (set-face-bold-p 'bold nil)
  ;; disable comment backgrounds
  (set-face-background 'font-lock-comment-face nil)

  ;; auto accept changes made to file if not changed in current buffer.
  (global-auto-revert-mode t)

  ;; auto-follow symlinks when editing
  (setq vc-follow-symlinks t)

  ;; style
  (setq powerline-default-separator 'bar)
  (setq org-bullets-bullet-list '("■" "◤" "▶" "●"))
  ;; helm
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  )
