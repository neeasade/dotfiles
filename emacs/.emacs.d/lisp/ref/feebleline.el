;;; feebleline.el --- Replace modeline with a slimmer proxy
;; neeasade fork to maek edits/import

;; Copyright 2018 Benjamin Lindqvist

;; Author: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; Maintainer: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; URL: https://github.com/tautologyclub/feebleline
;; Package-Version: 1.0
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For hardline Luddite editing!

;; Feebleline removes the modeline and replaces it with a slimmer proxy
;; version, which displays some basic information in the echo area
;; instead.  This information is only displayed if the echo area is not used
;; for anything else (but if you switch frame/window, it will replace whatever
;; message is currently displayed).

;; To customize feebleline's format, modify `feebleline-mode-line-text'.

;; NOTE:
;; feebleline.el will look considerably better with the following
;; settings:

;;   (window-divider-mode t)
;;   (setq window-divider-default-bottom-width 1)
;;   (setq window-divider-default-places (quote bottom-only))

;; But this mode does not work for all EMACS versions and may not work with
;; terminal EMACS (but I haven't checked).  If you're on GUI EMACS and your
;; version supports it, just place the following in your init file:

;;   (feebleline-default-settings)

;; Otherwise, do (feebleline-mode t) instead, but be warned that I'm not sure
;; if it will look good.

;;; Code:

(require 'advice)

(defvar feebleline-use-legacy-settings nil)
(when (< emacs-major-version 25)
  (setq feebleline-use-legacy-settings t))

(setq feebleline-use-legacy-settings nil)

(defface feebleline-time-face '((t :inherit 'font-lock-comment-face))
  "Feebleline timestamp face."
  :group 'feebleline)
(defface feebleline-linum-face '((t :inherit 'default))
  "Feebleline linum face."
  :group 'feebleline)
(defface feebleline-bufname-face '((t :inherit 'font-lock-function-name-face))
  "Feebleline filename face."
  :group 'feebleline)
(defface feebleline-asterisk-face '((t :foreground "salmon"))
  "Feebleline file modified asterisk face."
  :group 'feebleline)
(defface feebleline-previous-buffer-face '((t :foreground "#7e7e7e"))
  "Feebleline filename face."
  :group 'feebleline)
(defcustom feebleline-show-time nil
  "Set this if you want to show the time in the modeline proxy."
  :group 'feebleline)

(defun feebleline-previous-buffer-name ()
  "Get name of previous buffer."
  (buffer-name (other-buffer (current-buffer) 1)))

(defvar feebleline-mode-line-text nil
  "Each element is a list with the following format:

    (FORMAT-STRING FORMAT-ARGS PROPS)

FORMAT-STRING will be used as the first argument to `format', and
FORMAT-ARGS (a list) will be expanded as the rest of `format'
arguments.  If PROPS is given, it should be a list which will be
sent to `add-text-properties'.")

(setq feebleline-mode-line-text
      '(
        ("%s"       ((if feebleline-show-time (format-time-string "[%H:%M:%S] ") "")) (face feebleline-time-face))
        ("%6s"      ((format "%s,%s" (format-mode-line "%l") (current-column)))
         (face feebleline-linum-face))
        (" : %s"    ((if (buffer-file-name) (buffer-file-name)
                       (buffer-name))) (face feebleline-bufname-face))
        ("%s"       ((if (and (buffer-file-name) (buffer-modified-p)) "*" "" ))
         (face feebleline-asterisk-face))
        (" | %s"    ((feebleline-previous-buffer-name))
         (face feebleline-previous-buffer-face))))

(defun feebleline-default-settings-on ()
  "Some default settings that works well with feebleline."
  (custom-set-variables
   '(window-divider-default-bottom-width 1)
   '(window-divider-default-places (quote bottom-only)))
  (window-divider-mode t)
  (custom-set-variables '(mode-line-format nil)))

(defun feebleline-legacy-settings-on ()
  "Some default settings for EMACS < 25."
  (custom-set-faces '(mode-line ((t (:height 0.1))))))

(defvar feebleline/timer)
(defvar feebleline/mode-line-format-previous)

;;;###autoload
(define-minor-mode feebleline-mode
    "Replace modeline with a slimmer proxy."
  :require 'feebleline
  :global t
  (if feebleline-mode
      ;; Activation:
      (progn
        (setq feebleline/mode-line-format-previous mode-line-format)
        (setq feebleline/timer
              (run-with-timer 0 0.1 'feebleline-mode-line-proxy-fn))
        (if feebleline-use-legacy-settings (feebleline-legacy-settings-on)
          (feebleline-default-settings-on))
        (ad-activate 'handle-switch-frame)
        (add-hook 'focus-in-hook 'feebleline-mode-line-proxy-fn))

    ;; Deactivation:
    (custom-set-faces '(mode-line ((t :height unspecified))))
    (custom-set-variables
     '(mode-line-format feebleline/mode-line-format-previous))
    (cancel-timer feebleline/timer)
    (ad-deactivate 'handle-switch-frame)
    (remove-hook 'focus-in-hook 'feebleline-mode-line-proxy-fn)
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer))))

(defun feebleline--mode-line-part (part)
  "Return a PART (an element) of `feebleline-mode-line-text` as a propertized string."
  (let ((text (apply #'format (append (list (car part))
                                      (mapcar #'eval (cadr part)))))
        (props (elt part 2)))
    (when props
      (add-text-properties 0 (length text) props text))
    text))

(defvar feebleline-placeholder)
(defun feebleline-write-buffer-name-maybe ()
  "Replace echo-area message with mode-line proxy."
  (progn (setq feebleline-placeholder (mapconcat #'feebleline--mode-line-part
                                                 feebleline-mode-line-text ""))
         (with-current-buffer " *Minibuf-0*"
           (erase-buffer)
           (insert feebleline-placeholder))))

(defun feebleline-mode-line-proxy-fn ()
  "Put a mode-line proxy in the echo area *if* echo area is empty."
  (unless (current-message)
    (feebleline-write-buffer-name-maybe)))

(defadvice handle-switch-frame (after switch-frame-message-name)
  "Get the modeline proxy to work with i3 switch focus."
  (feebleline-write-buffer-name-maybe)
  ad-do-it
  (feebleline-write-buffer-name-maybe))

(provide 'feebleline)
;;; feebleline.el ends here
