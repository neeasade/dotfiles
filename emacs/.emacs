;; todo: look unto use-package
;;       tab/shift tab bindings for indent
;;       look at https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/init.el
;;       hydra + helm post.
;;       colorschemes?
;;       consolidate or get rid of swap files

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
			  'popup-complete
			  'better-defaults
			  'helm)

;; # KEYBINDS

;; C-; makes a good alternative toggle to shift-; I think.
(global-set-key (kbd "C-;") 'helm-M-x)

;; # MISC

;; hide tool bar.
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))

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

(require 'helm-config)

;; todo: see if reverting xst backspace patch alleviates this.
(global-set-key [(control h)] 'delete-backward-char)

;; offsets
(setq c-basic-offset 4)

;; no tabs (reference)
;; (setq-default indent-tabs-mode nil)

;; auto accept changes made to file if not changed in current buffer.
(global-auto-revert-mode t)

