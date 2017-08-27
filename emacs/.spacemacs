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

(defun get-resource (name default)
  "Get X resource value, with a fallback value."
  (if (executable-find "xrq")
      (let ((result
             ; shell-command-to-string appends newline
             (replace-regexp-in-string "\n$" ""
               (shell-command-to-string
                 (concat "xrq '" name "' 2>/dev/null")))))
        (if (string= result "")
            ; we didn't find it in xrdb.
            default
          result
          ))
    default
    ))

(defun dotspacemacs/layers ()
  (load-spacemacs-settings '(
      distribution 'spacemacs
      enable-lazy-installation 'unused
      ask-for-lazy-installation t
      configuration-layer-path '()
      frozen-packages '()
      excluded-packages '()
      install-packages 'used-only
      additional-packages '(
                            base16-theme
                            editorconfig
                            )
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
    vimscript

    ; interface
    (shell
     :variables
     shell-default-height 40
     shell-default-position 'top)

   better-defaults helm ranger

    ; features
    colors syntax-checking spell-checking
    (auto-completion
      :variables
        auto-completion-tab-key-behavior 'complete
        auto-completion-complete-with-key-sequence-delay 0
        auto-completion-enable-help-tooltip t
        company-quickhelp-delay 0.1)

    ; misc
    yaml
    themes-megapack
    evil-commentary
    git version-control
    org
  )))

(defun dotspacemacs/getfont()
  (get-resource "st.font" "Consolas-12")
  )

(defun dotspacemacs/init ()
  (load-spacemacs-settings '(
      startup-banner nil
      elpa-https t
      line-numbers nil
      elpa-timeout 5
      check-for-update nil
      elpa-subdirectory nil
      editing-style 'hybrid
      verbose-loading nil
      startup-lists '((recents . 5)
                      (projects . 7))

      startup-buffer-responsive t
      scratch-mode 'text-mode
      colorize-cursor-according-to-state nil

      themes (list (intern (get-resource "Emacs.theme" "spacemacs-dark")))

      default-font (list
                      ; xft format, match term.
                      (dotspacemacs/getfont)
                      :weight 'normal
                      :width 'normal
                      :powerline-scale (string-to-number (get-resource "Emacs.powerlinescale" "1.6")))

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
  (defconst custom-file (expand-file-name "custom.el" user-home-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file)
  )

(defun neeasade/style ()
  ; re-reference
  (dotspacemacs/init)

  (setq powerline-default-separator (get-resource "emacs.powerline" "bar"))

  (custom-set-faces
   '(spacemacs-normal-face ((t (:inherit 'mode-line)))))

  (set-face-background 'font-lock-comment-face nil)

  ; todo: make this on all frames, not just current
  (set-frame-parameter (selected-frame) 'internal-border-width
                       (string-to-number (get-resource "st.borderpx" "10")))

  ; sync w/ term background
  (set-background-color
   (get-resource "*.background"
                 (face-attribute 'default :background)))

  ; assume softer vertical border by matching comment face
  (set-face-attribute 'vertical-border
                      nil
                      :foreground (face-attribute 'font-lock-comment-face :foreground))

  ; this doesn't persist across new frames even though the docs say it should
  (set-face-attribute 'fringe nil :background nil)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-face-attribute 'fringe nil :background nil)
              )
            )

  ; set font on current and future
  (dotspacemacs/getfont)
  (set-face-attribute 'default nil :font (dotspacemacs/getfont))
  (set-frame-font (dotspacemacs/getfont) nil t)

  ; NO BOLD (set-face-bold-p doesn't cover everything, some fonts use slant and underline as bold...)
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :weight 'normal
                              :underline nil
                              :inherit nil
                              :slant 'normal))
        (face-list))

  ; done at end so it has correct font reference
  (spaceline-compile)
  )

(defun neeasade/org ()
  (setq org-bullets-bullet-list '("@" "%" ">" ">"))
  (setq org-directory "~/org")
  (setq org-startup-indented t)
  (setq org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE")))
  (setq org-blank-before-new-entry '((heading . t) (plainlist-item . nil)))
  (setq org-ellipsis "…")

  (setq org-clock-x11idle-program-name "x11idle")
  (setq org-clock-idle-time 10)
  (setq org-clock-sound nil)
  (setq org-pomodoro-play-sounds t)
  )


(defun dotspacemacs/user-config ()
  ;; auto accept changes made to file if not changed in current buffer.
  (global-auto-revert-mode t)

  ;; auto-follow symlinks when editing
  (setq vc-follow-symlinks t)

  ;; git
  (setq magit-repository-directories "~/git")

  ;; helm
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)

  ;; other
  (editorconfig-mode 1)

  (neeasade/style)
  (neeasade/org)
  )

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
