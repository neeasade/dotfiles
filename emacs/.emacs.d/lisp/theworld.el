
(defun init-use-package()
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))

  (package-initialize)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))
  (setq use-package-always-ensure t)
  )

(defun init-straight()
  (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
	(bootstrap-version 2))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  )

(defun neeasade/settings-sanity()
  ;; sanity
  (setq
   delete-old-versions -1
   vc-make-backup-files t
   backup-directory-alist `(("." . "~/.emacs.d/backups"))
   vc-follow-symlinks t ;; auto follow symlinks
   symlinked file
   auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
   inhibit-startup-screen t
   ring-bell-function 'ignore
   coding-system-for-read 'utf-8
   coding-system-for-write 'utf-8
   sentence-end-double-space nil
   initial-scratch-message ""
   global-auto-revert-mode t
   ;; consider
   version-control t
   )

  ;; trim gui
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; other
  (show-paren-mode 1)
  (blink-cursor-mode 0)

  (defconst custom-file "~/.emacs.d/custom.el")
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file)

  ;; allow things to load before we reload settings
  (setq desktop-restore-eager 0)
  (setq desktop-path (list "~/.emacs.d"))

  ;; retain session
  (desktop-save-mode 1)
  )

(defun neeasade/elisp()
  (load "~/.emacs.d/vendor/le-eval-and-insert-results.el")

  (setq lisp-indent-function 'common-lisp-indent-function)
  (neeasade/bind-leader-mode
   'emacs-lisp-mode
   "er" 'eval-region
   "ei" 'le::eval-and-insert-results
   "eb" 'le::eval-and-insert-all-sexps
   )
  )

;; zz behavior evil
(defun neeasade/evil()
  (use-package evil)
  (evil-mode 1)

  (add-function :after (symbol-function 'evil-scroll-line-to-center) #'neeasade/zz-scroll)
  (defun neeasade/zz-scroll(count)
    ;; window-total-size gets lines count when called with no args
    ;; note: this only works well for buffers that take more than the full screen...
    ;; also doesn't handle when buffer bottom is visible very well.
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


  (use-package evil-numbers)

  (use-package evil-leader
      :config
    (evil-leader/set-leader ",")
    (global-evil-leader-mode)
    )

  (use-package evil-surround   :config (global-evil-surround-mode 1))
  (use-package evil-commentary :config (evil-commentary-mode))
  (use-package evil-anzu) ;; displays current match and total matches.

  ;; defaults to fd/spacemacs-like config
  (use-package evil-escape :config (evil-escape-mode))

  ;; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
  (define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab] 'djoyner/evil-shift-left-visual)

  (defun djoyner/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun djoyner/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  )

(defun neeasade/flycheck()
  (use-package flycheck :config (global-flycheck-mode))

  (neeasade/bind
   ;; Applications
   "e" '(:ignore t :which-key "Errors")
   "en" 'flycheck-next-error
   "ep" 'flycheck-previous-error
   )
  )

(defun neeasade/company()
  ;; asdf config packages
  (use-package company
      :config
    (global-company-mode)
    (use-package company-flx
	:config
      (company-flx-mode +1))

    (setq
     company-idle-delay 0
     company-selection-wrap-around t
     company-tooltip-align-annotations t
     company-dabbrev-downcase nil
     company-dabbrev-ignore-case t
     company-tooltip-align-annotations t
     company-tooltip-margin 2
     )

    ;; TODO: investigate tab handling like VS completely
    (define-key company-active-map [tab] 'company-complete)
    )

  (use-package company-quickhelp
      :init
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 0.3)
    )
	)

(defun neeasade/editing()
  ;; TODO here: figure out how I want to sync indent styles across modes
  (use-package editorconfig)

  ;; from https://github.com/syl20bnr/spacemacs/blob/bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3/layers/%2Bdistributions/spacemacs-bootstrap/config.el
  ;; GPLv3
  (defvar spacemacs--indent-variable-alist
    ;; Note that derived modes must come before their sources
    '(((awk-mode c-mode c++-mode java-mode groovy-mode
	idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
      (python-mode . python-indent-offset)
      (cmake-mode . cmake-tab-width)
      (coffee-mode . coffee-tab-width)
      (cperl-mode . cperl-indent-level)
      (css-mode . css-indent-offset)
      (elixir-mode . elixir-smie-indent-basic)
      ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
      (enh-ruby-mode . enh-ruby-indent-level)
      (erlang-mode . erlang-indent-level)
      (js2-mode . js2-basic-offset)
      (js3-mode . js3-indent-level)
      ((js-mode json-mode) . js-indent-level)
      (latex-mode . (LaTeX-indent-level tex-indent-basic))
      (livescript-mode . livescript-tab-width)
      (mustache-mode . mustache-basic-offset)
      (nxml-mode . nxml-child-indent)
      (perl-mode . perl-indent-level)
      (puppet-mode . puppet-indent-level)
      (ruby-mode . ruby-indent-level)
      (rust-mode . rust-indent-offset)
      (scala-mode . scala-indent:step)
      (sgml-mode . sgml-basic-offset)
      (sh-mode . sh-basic-offset)
      (typescript-mode . typescript-indent-level)
      (web-mode . web-mode-markup-indent-offset)
      (yaml-mode . yaml-indent-offset))
    "An alist where each key is either a symbol corresponding
    to a major mode, a list of such symbols, or the symbol t,
    acting as default. The values are either integers, symbols
    or lists of these.")

  (defun spacemacs//set-evil-shift-width ()
    "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
    (let ((shift-width
	   (catch 'break
	     (dolist (test spacemacs--indent-variable-alist)
	       (let ((mode (car test))
		     (val (cdr test)))
		 (when (or (and (symbolp mode) (derived-mode-p mode))
			   (and (listp mode) (apply 'derived-mode-p mode))
			   (eq 't mode))
		   (when (not (listp val))
		     (setq val (list val)))
		   (dolist (v val)
		     (cond
		       ((integerp v) (throw 'break v))
		       ((and (symbolp v) (boundp v))
			(throw 'break (symbol-value v))))))))
	     (throw 'break (default-value 'evil-shift-width)))))
      (when (and (integerp shift-width)
		 (< 0 shift-width))
	(setq-local evil-shift-width shift-width))))

  (add-hook 'after-change-major-mode-hook 'spacemacs//set-evil-shift-width 'append)
  )

(defun neeasade/dashdocs()
  ;; todo: figure out ivy integration.
  ;; https://github.com/areina/helm-dash/issues/119
  (use-package help-dash)
  (setq helm-dash-browser-func 'eww-browse-existing-or-new)
  )

(defun spacemacs/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
		   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defun neeasade/style()
  (interactive)
  (use-package base16-theme)
  ;;(use-package ujelly-theme)

  (use-package spaceline
      :config
    (require 'spaceline-config)
    (setq powerline-scale (string-to-number (get-resource "Emacs.powerlinescale")))
    (setq powerline-height (spacemacs/compute-powerline-height))
    (spaceline-spacemacs-theme)
    )

  (load-theme (intern (get-resource "Emacs.theme")))
  (setq powerline-default-separator (get-resource "Emacs.powerline"))
  (set-face-attribute 'fringe nil :background nil)

  (setq powerline-default-separator (get-resource "emacs.powerline"))

  ;; sync modeline background color?
  ;;(set-face-attribute 'spacemacs-normal-face nil :inherit 'mode-line)

  (set-face-background 'font-lock-comment-face nil)

  ;; todo: make this on all frames, not just current
  (set-frame-parameter (selected-frame) 'internal-border-width
		       (string-to-number (get-resource "st.borderpx")))

  ;; sync w/ term background
  (set-background-color
   (get-resource "*.background"))

  ;; assume softer vertical border by matching comment face
  (set-face-attribute 'vertical-border
		      nil
		      :foreground (face-attribute 'font-lock-comment-face :foreground))

  ;; this doesn't persist across new frames even though the docs say it should
  (set-face-attribute 'fringe nil :background nil)
  (add-hook 'after-make-frame-functions
	    (lambda (frame)
	      (set-face-attribute 'fringe nil :background nil)
	      )
	    )

  ;; set font on current and future
  (set-face-attribute 'default nil :font (get-resource "st.font"))
  (set-frame-font (get-resource "st.font") nil t)

  ;; NO BOLD (set-face-bold-p doesn't cover everything, some fonts use slant and underline as bold...)
  (mapc (lambda (face)
	  (set-face-attribute face nil
			      :weight 'normal
			      :slant 'normal
			      :underline nil
					;:inherit nil
			      ))
	(face-list))

  ;; done at end so it has correct font reference
  (spaceline-compile)
  )

(defun neeasade/window-management()
  (use-package zoom
      :config
    (setq zoom-size '(0.58 . 0.618))
    (zoom-mode t)
    )
  )

(defun neeasade/org()
  (use-package org :config
    (load-settings
     "org"
     '(
       ;; where
       directory "~/org/projects"
       agenda-files (list org-directory)
       default-notes-file  "~/org/inbox.org"
       default-diary-file  "~/org/diary.org"
       default-habits-file  "~/org/habits.org"

       ;; style
       bullets-bullet-list '("@" "%" ">" ">")
       ellipsis "…"
       startup-indented t
       startup-folded nil

       ;; behavior
       ;; todo-keywords '((sequence "TODO" "NEXT" "WAITING" "INACTIVE" "CANCELLED" "MEETING" "DONE"))
       todo-keywords
       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	 (sequence "WAITING(w@/!)" "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)" "MEETING"))

       blank-before-new-entry '((heading . t) (plainlist-item . nil))
       tag-alist '(
		   ("test" . ?t)
		   ("endtest" . ?e)
		   )

       ;; clock
       clock-x11idle-program-name "x11idle"
       clock-idle-time 10
       clock-sound nil
       pomodoro-play-sounds nil
       pomodoro-keep-killed-pomodoro-time t
       pomodoro-ask-upon-killing nil

       ;; capture
       capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)

	 ("b" "Blank" entry (file org-default-notes-file)
	  "* %?\n%u")

	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)

	 ("d" "Diary" entry (file+datetree org-default-diary-file)
	  "* %?\n%U\n" :clock-in t :clock-resume t)

	 ("D" "Daily Log" entry (file "~/org/daily-log.org")
	  "* %u %?\n*Summary*: \n\n*Problem*: \n\n*Insight*: \n\n*Tomorrow*: " :clock-in t :clock-resume t)

	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%u" :clock-in t :clock-resume t)

	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t")
	 )

       ;; current file or any of the agenda-files, max 9 levels deep
       refile-targets '(
			(nil :maxlevel . 9)
			(org-agenda-files :maxlevel . 9)
			)
       )
     )
    )

  (use-package evil-org
      :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
	      (lambda ()
		(evil-org-set-key-theme))))

  (add-hook
   'org-mode-hook
   ;; Configure leader key
   (neeasade/bind-leader-mode
    'org-mode
    "t" 'org-todo
    "T" 'org-show-todo-tree
    "v" 'org-mark-element
    "a" 'org-agenda
    "c" 'org-archive-subtree
    "l" 'evil-org-open-links
    "C" 'org-resolve-clocks
    )

   (evil-define-key 'normal evil-org-mode-map
     "t" 'org-todo
     )
   )

  (use-package org-pomodoro :config
    (add-hook 'org-pomodoro-started-hook
	      (apply-partially #'shell-command "player.sh play"))

    (add-hook 'org-pomodoro-break-finished-hook
	      (apply-partially #'shell-command "player.sh play"))

    (add-hook 'org-pomodoro-finished-hook
	      (apply-partially #'shell-command "player.sh pause"))
    )

  (neeasade/bind
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gb" 'magit-blame
   "gl" 'magit-log-current
   )

  )

;; TODO: experiment with centered placement here
(defun neeasade/ivy-style()
  (use-package oneonone)
  )

(defun neeasade/clojure()
  (use-package clojure-mode)
  (use-package cider)
  ;; (evil-leader/set-key-for-mode
  ;;   '
  ;;   "er" 'eval-region
  ;;   "ei" 'le::eval-and-insert-results
  ;;   "eb" 'le::eval-and-insert-all-sexps
  ;; )
  )

(defun neeasade/nix()
  (use-package nix-mode)
  )

(defun dynamic-ivy-height()
  "Placeholder."
  (let (
	(computed (/ (window-total-size) 2))
	)
    (setq ivy-height computed)
    (setq ivy-fixed-height-minibuffer computed)
    )
  )

;; bindings, ivy, counsel, alerts, which-key
(defun neeasade/interface()
  ;; ref: http://oremacs.com/2016/01/06/ivy-flx/
  (use-package flx)

  (dynamic-ivy-height)
  (add-hook 'window-configuration-change-hook 'dynamic-ivy-height)

  (use-package ivy
      :config
    (setq ivy-re-builders-alist
	  '((ivy-switch-buffer . ivy--regex-plus)
	    (t . ivy--regex-fuzzy)))

    (setq ivy-initial-inputs-alist nil)
    (ivy-mode 1))

  ;; counsel
  (use-package counsel
      :bind
    ("C-c k" . counsel-ag))

  (use-package general
      :config
    ;; bind a key globally in normal state
    (setq general-default-keymaps 'evil-normal-state-map)
    )

  (neeasade/bind
   ;; simple command
   "'"   '(iterm-focus :which-key "iterm")
   "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
   "/"   'counsel-ag
   "TAB" '(switch-to-other-buffer :which-key "prev buffer")
   "SPC" 'counsel-M-x

   ;; windows
   "w" '(:ignore t :which-key "Windows")
   "wh" 'evil-window-left
   "wj" 'evil-window-down
   "wk" 'evil-window-up
   "wl" 'evil-window-right
   "wd" 'evil-window-delete

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ad" 'dired
   )

  (use-package alert
    :config (setq alert-default-style 'libnotify))

  (use-package which-key
      :config
    (which-key-setup-side-window-right-bottom)
    (setq
     which-key-sort-order 'which-key-key-order-alpha
     which-key-idle-delay 0.4
     which-key-side-window-max-width 0.33
     )
    (which-key-mode)
    )
  )

;; TODO: figure out mpd integration here
(defun neeasade/emms()
  (use-package emms)
  )

(defun neeasade/projectile()
  (use-package projectile)
  ;; (project-find-file-in)
  (neeasade/bind

   "p" '(:ignore t :which-key "projects")
   "pf" 'project-find-file
   ;; "ad" 'dired
   )
  )

(defun neeasade/git()
  (use-package magit
      :config
    (setq magit-repository-directories (list "~/git"))
    )

  (use-package evil-magit
      :config
    (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)
    )

  (neeasade/bind
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gb" 'magit-blame
   "gl" 'magit-log-current
   )
  )

(defun neeasade/dumbjump()
  (use-package dumb-jump
      :config
    (setq dumb-jump-selector 'ivy)
    (neeasade/bind
     "j" '(:ignore t :which-key "Jump")
     "jj" 'dumb-jump-go
     "jb" 'dumb-jump-back
     )
    )
  )

(defun neeasade/irc()
  (use-package circe
      :config
    (setq circe-network-options
	  `(
	    ("Freenode"
	     :tls t
	     :nick "neeasade"
	     :nickserv-password ,(pass "freenode")
	     :channels (:after-auth "#bspwm")
	     )
	    ))
    )


  (defun circe-network-connected-p (network)
    "Return non-nil if there's any Circe server-buffer whose `circe-server-netwok' is NETWORK."
    (catch 'return
      (dolist (buffer (circe-server-buffers))
	(with-current-buffer buffer
	  (if (string= network circe-server-network)
	      (throw 'return t))))))

  (defun circe-maybe-connect (network)
    "Connect to NETWORK, but ask user for confirmation if it's already been connected to."
    (interactive "sNetwork: ")
    (if (or (not (circe-network-connected-p network))
	    (y-or-n-p (format "Already connected to %s, reconnect?" network)))
	(circe network)))

  (defun connect-all-irc()
    (interactive)
    (circe-maybe-connect "Freenode")
    ;;(circe-maybe-connect "Rizon")
    )

  ;; channel name in prompt
  (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
  (defun my-circe-prompt ()
    (lui-set-prompt
     (concat (propertize (concat (buffer-name) ">") 'face 'circe-prompt-face) " ")))

  ;; prevent too long pastes/prompt on it:
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

  ;; hide part, join, quit
  (setq circe-reduce-lurker-spam t)

  (setq circe-format-say "{nick:-8s} {body}")

  (load "lui-logging" nil t)
  (setq lui-logging-directory "~/.irc")
  (enable-lui-logging-globally)

  (setq
   lui-time-stamp-position 'right-margin
   lui-time-stamp-format "%H:%M")

  (add-hook 'lui-mode-hook 'my-circe-set-margin)
  (defun my-circe-set-margin ()
    (setq right-margin-width 5))

  ;; fluid width windows
  (setq
   lui-time-stamp-position 'right-margin
   lui-fill-type nil)

  (add-hook 'lui-mode-hook 'my-lui-setup)
  (defun my-lui-setup ()
    (setq
     fringes-outside-margins t
     right-margin-width 5
     word-wrap t
     wrap-prefix "    ")
    (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)
    )

  (setq lui-highlight-keywords (list "neeasade"))
  ;; TODO consider: a binding/function to search open channels
  (neeasade/bind
   "ai" 'connect-all-irc
   )
  )
