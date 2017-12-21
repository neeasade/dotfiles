(defun mapcar* (f &rest xs)
  "MAPCAR for multiple sequences F XS."
  (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
	    (apply 'mapcar* f (mapcar 'cdr xs)))))

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

;; wrap passwordstore
(defun pass (key)
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (concat "pass " key " 2>/dev/null")))
  )

(defun reload-init()
  "Reload init.el."
  (interactive)
  (straight-transaction
      (straight-mark-transaction-as-init)
      (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

;; if eww is displayed, use that, else open here.
;; todo: this isn't working with anchors in other frames
(defun eww-browse-existing-or-new (url)
    (if (get-buffer-window "*eww*" 0)
          (url-retrieve url 'eww-render
                        (list url nil (get-buffer "*eww*")))
        (eww url)
      )
 )

;; binding wrappers
(defun neeasade/bind (&rest binds)
  (apply 'general-define-key :prefix "SPC" binds)
  )

;; makes assumption keymap for mode will be named <modename>-map,
;; per https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html
(defun neeasade/bind-mode(mode &rest binds)
  (apply 'general-define-key
	 :keymaps (intern (concat (symbol-name mode) "-mode"))  binds)
  )

(defun neeasade/bind-leader (mode &rest binds)
  (apply 'evil-leader/set-key-for-mode mode binds)
  )

(defun neeasade/bind-leader-mode(mode &rest binds)
  (apply 'evil-leader/set-key-for-mode mode binds)
  ) 
