;;; init.el --- neeasade
;;; commentary:
;;; See the bottom.
;;; code:

;; helpers
(defun mapcar* (f &rest xs)
  "MAPCAR for multiple sequences F XS."
  (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
	    (apply 'mapcar* f (mapcar 'cdr xs)))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
			 (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; a namespace variable setting function
(defun load-settings(namespace lst)
  "Set dotspacemacs- prefixed variable values from a list."
  (require 'seq)
  (mapcar*
   (lambda (pair)
     (let ((key (car pair))
	   (value (car (cdr pair))))
       (set-default
	(intern (concat namespace "-" (prin1-to-string key)))
	(eval value)
	)))
   (seq-partition lst 2)
   ))

;; for when we're away from $HOME.
(defvar xrdb-fallback-values
  '(
    ("Emacs.theme"          . "base16-grayscale-light")
    ("Emacs.powerlinescale" . "1.4")
    ("st.font"              . "Consolas-12")
    ("st.borderpx"          . "10")
    ("emacs.powerline"      . "bar")
    ("*.background"         . (face-attribute 'default :background))
    ))

(defun get-resource (name)
  "Get X resource value, with a fallback value NAME."
  (let ((default (eval (cdr (assoc name xrdb-fallback-values)))))
    (if (executable-find "xrq")
	(let ((result
	       ;; shell-command-to-string appends newline
	       (replace-regexp-in-string "\n$" ""
					 (shell-command-to-string
					  (concat "xrq '" name "' 2>/dev/null")))))
	  (if (string= result "")
	      ;; we didn't find it in xrdb.
	      default
	    result
	    ))
      default
      )))

(defun reload-init()
  "Reload init.el."
  (interactive)
  (straight-transaction
      (straight-mark-transaction-as-init)
      (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(defun neeasade/bind (&rest binds)
  "Global prefix spot for BINDS."
  (apply 'general-define-key :prefix "SPC" binds)
  )

(defun neeasade/bindmode(mode &rest binds)
  (apply 'general-define-key :keymaps mode binds)
  )

;; The content:
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

  ;; allow things to load before we reload settings
  (setq desktop-restore-eager 0)
  (setq desktop-path (list "~/.emacs.d"))

  ;; retain session
  (desktop-save-mode 1)
  )

(defun neeasade/elisp()
  (load "~/.emacs.d/vendor/le-eval-and-insert-results.el")

  (setq lisp-indent-function 'common-lisp-indent-function)
  (evil-leader/set-key-for-mode
      'emacs-lisp-mode
      "er" 'eval-region
      "ei" 'le::eval-and-insert-results
      "eb" 'le::eval-and-insert-all-sexps
      )
  )

;; zz behavior evil
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

(defun neeasade/evil()
  (use-package evil
      :config (evil-mode 1)
      (add-function :after (symbol-function 'evil-scroll-line-to-center) #'myscroll)
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
     company-dabbrev-downcase 0
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
  )

(defun spacemacs/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
		   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defun neeasade/style()
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
    (load-settings "org"
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
   (evil-leader/set-key-for-mode 'org-mode
       "t" 'org-todo
       "T" 'org-show-todo-tree
       "v" 'org-mark-element
       "a" 'org-agenda
       "c" 'org-archive-subtree
       "l" 'evil-org-open-links
       "C" 'org-resolve-clocks)

   (neeasade/bindmode
    'org-mode-map
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
  )

;; TODO: experiment with centered placement here
(defun neeasade/ivy-style()
  (use-package oneonone)
  )

(defun neeasade/clojure()
  (use-package cider)
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
   )
  )

;; (init-use-package)
(init-straight)

(defun neeasade/core()
  (neeasade/settings-sanity)
  (neeasade/evil)
  (neeasade/interface)
  (neeasade/editing)
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
  )

(defun neeasade/communication()
  ;; TODO: irc, email, slack.
  )

(defun neeasade/development()
  (neeasade/clojure)
  (neeasade/elisp)
  )

(neeasade/core)
(neeasade/extra)
(neeasade/communication)
(neeasade/development)

(provide 'init)

;;; init.el ends here

