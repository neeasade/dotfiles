;; todo: look unto use-package
;;       look at https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/init.el
;;       hydra + helm post.
;;       consolidate or get rid of swap files
;;       reorg this.
;;       review links/literate emacs configs.
;;       package install

;; # PACKAGES
(require 'package)

;; add repos
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(setq package-enable-at-startup nil)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, if not, install it."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
	(package-install package)
     package))
   packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; activate installed packages
(package-initialize)

;; get the thing.
(ensure-package-installed 'magit
			  'projectile
			  'evil
			  'git-gutter
			  'spaceline
			  'popup-complete
			  'better-defaults
			  'whitespace-cleanup-mode
			  'helm)

;; # KEYBINDS

;; indenting, ref: http://stackoverflow.com/questions/2249955/emacs-shift-tab-to-left-shift-the-block
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
  (indent-region -4)
  )

(defun tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (indent-region 4) ; region was selected, call indent-region
    (indent-line-to (+ (current-indentation) 4)) ; else indent line by 4
    )
  )

(defun tab-region-inplace (N)
  (interactive "p")
  (if (use-region-p)
      (indent-region 4) ; region was selected, call indent-region
    (insert "    ") ; insert 4 spaces
    )
  )


;; # MISC

(global-whitespace-cleanup-mode)

;; always visible cursor:
(when (fboundp #'blink-cursor-mode)
  (blink-cursor-mode -1))

(setq initial-scratch-message ";; scratch buffer\n")

;; mute start screen
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))
;; todo: understand:
(setq inhibit-startup-screen t)
(setq gc-cons-threshold 100000000)
(setq file-name-handler-alist nil)

;; auto-toggle evil major mode
(require 'evil)
(evil-mode t)

(global-set-key (kbd "<backtab>") 'untab-region)

(with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "TAB") 'tab-region)
    (define-key evil-visual-state-map (kbd "TAB") 'tab-region)
    (define-key evil-insert-state-map (kbd "TAB") 'tab-region-inplace))

(require 'helm-config)

;; offsets
(setq c-basic-offset 4)

;; no tabs (reference)
; (setq-default indent-tabs-mode nil)

;; global company mode
;; TODO: look into options, popups are slow it feels.
(add-hook 'after-init-hook 'global-company-mode)

;; auto accept changes made to file if not changed in current buffer.
(global-auto-revert-mode t)

;; steal spacemacs mode line 
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; auto follow symbolic links
(setq vc-follow-symlinks t)

;; git gutter
(global-git-gutter-mode +1)

;; note: depends on emacs 24.4
;; auto save on unfocus
(add-hook 'focus-out-hook 'save-buffer)
