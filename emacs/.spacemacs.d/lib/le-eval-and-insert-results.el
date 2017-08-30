;; -*- lexical-binding: t -*-
;;; le-eval-and-insert-results.el --- evaluates buffer and inline results

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: evaluates buffer and inline results
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Tue Sep 13 01:04:33 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Sun Jun 16 21:52:35 2013 (+0800)
;;           By: Le Wang
;;     Update #: 117
;; URL: https://github.com/lewang/le_emacs_libs/blob/master/le-eval-and-insert-results.el
;; Keywords: emacs-lisp evaluation
;; Compatibility: Emacs 23+

;;; Installation:

;;   (require 'le-eval-and-insert-results)
;;
;; M-x le::eval-and-insert-results
;;

;;; Commentary:

;; Simple function to evaluate buffer and inline results of each top-level
;; form as a comment.
;;
;; This is basically batch ielm.  I find it useful when experimenting with
;; stuff.
;;
;; Function is reentrant, and should update results each time.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(provide 'le-eval-and-insert-results)
	;;; ⇒ le-eval-and-insert-results

(defun le::eair::format-res (type str)
  (let ((triple-comment (make-string 3 (string-to-char comment-start))))
    (apply 'concat
           (nconc
            (list
             "\t"
             triple-comment
             " ⇒ ")
            (when type
              (list
               "<"
               (symbol-name type)
               ">"
               "\n"
               "\t"
               triple-comment
               "   "))
            (list
             (replace-regexp-in-string
              "\n"
              (concat
               "\n\t"
               triple-comment
               "   ")
              (replace-regexp-in-string
               "\n\\'"
               ""
               str)))))))

(defun le::eair::format-eval-output (res)
  (if (hash-table-p res)
      (mapconcat
       'identity
       (nconc (cl-reduce
               (lambda (accum k)
                 (let ((res (gethash k res)))
                   (if res
                       (nconc
                        accum
                        (list
                         (le::eair::format-res
                          (if (eq k 'value)
                              nil
                            k)
                          res)))
                     accum)))
               '(value out err)
               :initial-value ())
              '(""))
       "\n")
    (le::eair::format-res nil res)))

(defun le::eval-and-insert-all-sexps (beg end)
  "call `le::eval-and-insert-results' for all sexps in region.

With universal arguments, use whole buffer.
"
  (interactive (cond ((use-region-p)
                      (list (region-beginning) (region-end)))
                     ((consp current-prefix-arg)
                      (list (point-min) (point-max)))
                     (t
                      (error "Press C-u to use entire buffer, bro."))))
  (setq end (save-excursion
              (goto-char end)
              (skip-chars-backward " \t\n")
              (forward-comment -10000)
              (copy-marker (point))))
  (let ((forward-func (nth 1 (le::eair::get-mode-info))))
    (save-excursion
      (goto-char beg)
      (loop do (progn
                 (comment-forward 10000)
                 (funcall forward-func 1)
                 (call-interactively 'le::eval-and-insert-results))
            while (and (< (point) end)
                       (not (eobp))))))
  (set-marker end nil))

(defun le::eair::get-result-regexp ()
  "get regexp for current buffer"
  (let ((tab-space (make-string tab-width ? ))
        (triple-comment (make-string 3 (string-to-char comment-start))))
    (concat "\\(?:\t\\|"
            tab-space
            "\\)"
            triple-comment
            " ⇒.*\n?\\(?:\\(\t\\|"
            tab-space
            "\\)"
            triple-comment
            " .*\n\\)*")))

;;;###autoload
(defun le::clear-inserted-results (beg end)
  "clear inserted results in region."
  (interactive "*r")
  (save-excursion
    (let ((case-fold-search nil)
          (regexp (le::eair::get-result-regexp)))
      (setq end (copy-marker end t))
      (goto-char beg)
      (while (re-search-forward regexp end 'noerror)
        (replace-match ""))
      (set-marker end nil))))


(defvar le::eair::modes-alist
  '((clojure-mode          backward-sexp forward-sexp nil)
    (emacs-lisp-mode       backward-sexp forward-sexp nil)
    (lisp-interaction-mode beginning-of-defun end-of-defun nil)
    (sql-mode              backward-paragraph forward-paragraph le::eair::sql-ok)
    (t                     backward-paragraph forward-paragraph le::eair::no-context))
  "Per major-mode settings.
  (BACKWARD-FUNC FORWARD-FUNC VALIDATION-func)")

(defun le::eair::get-mode-info ()
  "return list of movements for current major-mode"
  (let ((res (assq major-mode le::eair::modes-alist)))
    (cdr
     (or res
         (assq t le::eair::modes-alist)))))

(defun le::eair::result-handler-maker (pos)
  "create a function appropriate for handling result"
  (let ((marker (copy-marker pos)))
    (lambda (res)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (insert (le::eair::format-eval-output res))))
      (set-marker marker nil))))

(defun le::eair::no-context ()
  "return t when context is not in the middle of string or paired
  delimiter.

  Raise Error otherwise."
  (let ((info (syntax-ppss)))
    ;; (1 3321 3351 nil nil nil 0 nil nil
    ;;    (3321))
    (cond ((not (eq 0 (nth 0 info)))
           (error "Unfinished list."))
          ((nth 3 info)
           (error "Unfinished string."))
          (t))))

(defun le::eair::sql-ok ()
  (and (le::eair::no-context)
       (save-excursion
         (skip-chars-backward "\n\t ")
         (if (= ?\; (char-before))
             t
           (error "Missing semi-colon.")))))

(defun le::evair-process-region ()
  "Process raw BEG END for interactive use."
  (destructuring-bind (back-func forward-func validation)
      (le::eair::get-mode-info)
    (let ((saved-point (point))
          (initial-beg (progn
                         (funcall back-func)
                         (point)))
          (initial-end (progn
                         (funcall forward-func)
                         (forward-comment 10000)
                         (point))))
      (le::clear-inserted-results initial-beg initial-end)
      (goto-char initial-beg)
      (skip-chars-forward " \t\n")
      (prog1
          (list (point-at-bol 1)
                (progn
                  (funcall forward-func)
                  (skip-chars-backward " \t")
                  (unless (bolp)
                    (if (eobp)
                        (insert "\n")
                      (forward-line 1)))
                  (unless (looking-at-p "^[ \t]*$")
                    (insert "\n")
                    (forward-line -1))

                  (when validation
                    (funcall validation))
                  (point)))
        (goto-char saved-point)))))

;;;###autoload
(defun le::eval-and-insert-results (beg end)
  "eval region as single form and append result to end."
  (interactive (le::evair-process-region))
  (let* ((sexp-str (buffer-substring-no-properties beg end))
         (insert-pos end))
    (le::eval-and-insert-sexp sexp-str insert-pos)))

;;;###autoload
(defun le::eval-and-insert-toplevel-results (&optional beg end)
  "Evaluate top level form and append resutl as comment.

When region is active, do so for all forms in region (ignore levels)."
  (interactive (when (use-region-p)
                 (let ((beg (region-beginning))
                       (end (region-end)))
                   (save-excursion
                     ;; find precise beginning and ending of first/last sexp
                     (list (progn (goto-char beg)
                                  (forward-sexp)
                                  (backward-sexp)
                                  (point))
                           (progn (goto-char end)
                                  (backward-sexp)
                                  (forward-sexp)
                                  (point)))))))
  (if end
      (save-excursion
        (goto-char beg)
        (let ((end (copy-marker end t)))
          (while (< (point) end)
            (forward-sexp)
            (call-interactively 'le::eval-and-insert-results))))
    (end-of-defun)
    (call-interactively 'le::eval-and-insert-results)))

(defun le::evair-nrepl-handler (handler)
  (let ((res (make-hash-table)))
    (lambda (response)
      ;; response starts with a 'dict
      (when (eq (car response) 'dict)
        (setq response (cdr response))
        (loop for key in '("value" "out" "err")
              for val = (lax-plist-get response key)
              if val
              do (let ((key (intern key)))
                   (puthash key (concat (gethash key res) val) res))))
      (when (equal (car (lax-plist-get response "status")) "done")
        (funcall handler res)))))

(defun le::eval-and-insert-sexp (sexp-str insert-pos)
  "eval str and insert results at insert-pos."
  (let ((handler (le::eair::result-handler-maker insert-pos)))
    (case major-mode
      ('clojure-mode
       (nrepl-send-string sexp-str
                          (le::evair-nrepl-handler handler)))
      ((emacs-lisp-mode lisp-interaction-mode) ; emacs-lisps
       (funcall handler (prin1-to-string
                         (eval (read sexp-str)))))
      (t
       (require 'le-comint-gather-output)
       (le::comint-get-output sexp-str handler)))))

;;; testing
(ignore
 (quote

  (let ((hash (make-hash-table)))
    (puthash 'value "abc" hash)
    (le::eair::format-eval-output hash))

  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; le-eval-and-insert-results.el ends here
