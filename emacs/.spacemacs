;; -*- mode: emacs-lisp -*-
;; options reference: https://github.com/syl20bnr/spacemacs/blob/master/core/templates/.spacemacs.template

(defun split-list (lst)
  "Split a list down the middle, into 2 lists."
  (if lst
      (if (cddr lst)
          (let ((l (split-list (cddr lst))))
            (list
             (cons (car lst) (car l))
             (cons (cadr lst) (cadr l))))
        `((,(car lst)) ,(cdr lst)))
    '(nil nil)))

(defun load-spacemacs-settings(lst)
  "Set dotspacemacs- prefixed variable values from a list."
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
      frozen-packages '()
      excluded-packages '()
      install-packages 'used-only
      additional-packages '( base16-theme )
    ))

  ;; setting this here allows spacemacs to add layers.
  (setq dotspacemacs-configuration-layers '(
    ; languages
    c-c++ clojure
    emacs-lisp
    html
    javascript
    markdown
    nixos
    rust
    (typescript
      :variables
        typescript-fmt-on-save t)
    (shell
      :variables
        shell-default-height 30
        shell-default-position 'bottom)

    ; interface
    better-defaults helm ranger

    ; features
    colors syntax-checking spell-checking
    (auto-completion
      :variables
        auto-completion-tab-key-behavior 'complete
        auto-completion-complete-with-key-sequence-delay 0
    )

    ; misc
    yaml
    themes-megapack
    evil-commentary
    git version-control
    org
  )))

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
      ;; todo: conditionally fallback if xrq isn't found.
      themes '(base16-ocean)
      default-font '("Consolas"
                      :size 12
                      :weight normal
                      :width normal
                      :powerline-scale 1.6)
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
  ;; todo: make the file if it doesn't exist
  (setq custom-file (file-truename (concat dotspacemacs-directory ".spacemacs-custom")))
  (load custom-file)
  )

(defun dotspacemacs/user-config ()
  ;; auto accept changes made to file if not changed in current buffer.
  (global-auto-revert-mode t)

  ;; auto-follow symlinks when editing
  (setq vc-follow-symlinks t)

  ;; helm
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)

  ;; style options
  (setq powerline-default-separator 'bar)
  (spaceline-compile)
  (setq org-bullets-bullet-list '("■" "◤" "▶" "●"))
  (set-face-bold-p 'bold nil)
  (set-face-background 'font-lock-comment-face nil)
  )
