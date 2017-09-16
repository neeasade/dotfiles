;; -*- mode: emacs-lisp -*-

; a namespace variable setting function
(defun load-settings(namespace lst)
  "Set dotspacemacs- prefixed variable values from a list."
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

; for when we're away from $HOME.
(defvar xrdb-fallback-values '(
  ("Emacs.theme"          . "base16-grayscale-light")
  ("Emacs.powerlinescale" . "1.4")
  ("st.font"              . "Consolas-12")
  ("st.borderpx"          . "10")
  ("emacs.powerline"      . "bar")
  ("*.background"         . (face-attribute 'default :background))
  ))

(defun get-resource (name)
  "Get X resource value, with a fallback value."
  (let ((default (eval (cdr (assoc name xrdb-fallback-values)))))
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
      )))

; used in others.
(defun load-from (dir targets)
  (dolist (target (split-string targets " "))
    (load (concat "~/.spacemacs.d/" dir "/" target ".el"))
    )
  )
