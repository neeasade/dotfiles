;;; init.el --- dotmacs
;;; Commentary: none.
;;; Code:

;; referenced: https://github.com/aaronbieber/dotfiles/blob/master/configs/emacs.d/init.el

;; bootstrap use-package.
(package-initialize)

;; activate installed packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-check-signature nil)

;; add repos
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'default-frame-alist
             '(font . "Droid Sans Mono-9"))

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
; disable the scrollbar
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
; highlights matching parens
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(put 'narrow-to-region 'disabled nil)
; improves cursor performance on windows as far as I can tell
; ref: http://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)
; auto complete parens
(electric-pair-mode 1)

(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
(setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; asdf config packages
(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (use-package company-flx
    :ensure t
    :config
    (company-flx-mode +1))

  (setq company-idle-delay 0.5)
  (setq company-selection-wrap-around t)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map [tab] 'company-complete)
  ;; todo: consider these keybindings/investigate tab handling like VS
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1))

(use-package base16-theme
  :ensure t)

(defun air--config-evil-leader ()
  ; todo: consider adding this/refine.
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    "b"  'helm-mini             ;; Switch to another buffer
    "B"  'magit-blame-toggle
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "D"  'open-current-line-in-codebase-search
    "f"  'helm-imenu            ;; Jump to function in buffer
    "g"  'magit-status
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'whitespace-mode       ;; Show invisible characters
    "nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    "nw" 'widen
    "o"  'delete-other-windows  ;; C-w o
    "p"  'helm-show-kill-ring
    "s"  'ag-project            ;; Ag search from project's root
    "r"  'chrome-reload
    "R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "t"  'gtags-reindex
    "T"  'gtags-find-tag
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame))))

(defun air--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  ;(add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  (define-key evil-normal-state-map (kbd "<down>")  'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<up>")    'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-")       'helm-find-files)
  ;(define-key evil-normal-state-map (kbd "C-]")     'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")      'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")      'show-first-occurrence)
  (define-key evil-normal-state-map (kbd "S-SPC")   'air-pop-to-org-agenda)
  ;(define-key evil-insert-state-map (kbd "C-e")     'end-of-line) ;; I know...
  (define-key evil-motion-state-map (kbd "TAB") 'tab-region)
  (define-key evil-visual-state-map (kbd "TAB") 'tab-region)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-region-inplace)


  ;; todo: investigate projectile, counsel/ivy integration with it.
  ;(evil-define-key 'normal global-map (kbd "C-p")   'helm-projectile)
  ;(evil-define-key 'normal global-map (kbd "C-S-p") 'helm-projectile-switch-project)
  ;; todo: consider this binding..
  ;(evil-define-key 'insert global-map (kbd "s-d")   'eval-last-sexp)
  (evil-define-key 'normal global-map (kbd "s-d")   'eval-defun)
  (evil-define-key 'normal global-map (kbd "C-t")   (lambda () (interactive) (ansi-term (getenv "SHELL"))))

  (evil-define-key 'normal global-map (kbd "z d")   'dictionary-lookup-definition)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
)

;; indenting functions, later bound to {shift + }tab.
;; ref: http://stackoverflow.com/questions/2249955/emacs-shift-tab-to-left-shift-the-block
;; todo: reinvestigate these bindings.
(defun indent-region(numSpaces)
  (progn
    ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

    ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion ; restore the position afterwards
      (goto-char regionStart) ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd) ; go to the end of region
      (setq end (line-end-position)) ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil) ; restore the selected region
      )
    )
  )

(defun untab-region (N)
  (interactive "p")
  (indent-region -2)
  )

(defun tab-region (N)
    (interactive "p")
    (if (use-region-p) ;else
        (indent-region 2) ; region was selected, call indent-region
      (indent-line-to (+ (current-indentation) 2)) ; else indent line by 4
      )
    )

(defun tab-region-inplace (N)
  (interactive "p")
  (if (use-region-p)
      (indent-region 2) ; region was selected, call indent-region
    (insert "  ") ; insert 4 spaces
    )
  )

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  ;; todo: consider
  (use-package evil-indent-textobject
    :ensure t))

(use-package nlinum
  :ensure t
  :config
  (use-package nlinum-relative
      :ensure t
      :config
      ;; something else you want
      (eval-after-load "evil" '(nlinum-relative-setup-evil))
      (add-hook 'prog-mode-hook 'nlinum-relative-mode)
      (setq nlinum-relative-redisplay-delay 0)      ;; delay
      (setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
      (global-nlinum-relative-mode)
      ))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

(use-package zenburn-theme :ensure t :defer t)
(use-package git-gutter
    :ensure t
    :defer t
    :config
    (global-git-gutter-mode +1))

(use-package whitespace-cleanup-mode
    :ensure t
    :defer t
    :config
    (global-whitespace-cleanup-mode))

(use-package let-alist
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))

  ;; Flycheck mode:
  (add-hook 'flycheck-mode-hook
            (lambda ()
                (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
                (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))))

;; package to beautify web junk
(use-package web-beautify
  :ensure t)

;; typescript integration
(use-package tide
  :ensure t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (setq tide-completion-ignore-case t)
    (setq typescript-indent-level 2)
    (global-set-key (kbd "<f12>") 'tide-jump-to-definition)
      (company-mode +1))
    (setq tide-format-options
          '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
            :placeOpenBraceOnNewLineForFunctions nil))

    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config

  ;; ref: http://oremacs.com/2016/01/06/ivy-flx/
  (use-package flx
    :ensure t)
  ;; ref: https://www.reddit.com/r/emacs/comments/3xzas3/help_with_ivycounsel_fuzzy_matching_and_sorting/
  (setq ivy-re-builders-alist
    '((ivy-switch-buffer . ivy--regex-plus)
      (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)

  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; todo: evaluate these.
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-load-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  (ivy-mode 1))

;; auto set #! files to be executable.
(add-hook 'after-save-hook
        #'(lambda ()
        (and (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (save-match-data
                   (looking-at "^#!"))))
             (not (file-executable-p buffer-file-name))
             (shell-command (concat "chmod u+x " buffer-file-name))
             (message
              (concat "Saved as script: " buffer-file-name)))))

;; # KEYBINDS

;; always visible cursor:
(when (fboundp #'blink-cursor-mode)
  (blink-cursor-mode -1))

;; hm..
(global-set-key (kbd "<backtab>") 'untab-region)

;; offsets
;; (setq c-basic-offset 4)

;; no tabs (reference)
; (setq-default indent-tabs-mode nil)

;; auto accept changes made to file if not changed in current buffer.
(global-auto-revert-mode t)

(provide '.emacs )
;;; .emacs ends here
