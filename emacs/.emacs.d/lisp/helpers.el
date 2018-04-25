(setq sys/windows? (eq system-type 'windows-nt))
(setq sys/linux? (eq system-type 'gnu/linux))
(setq enable-tp? sys/windows?)

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
       (set
	(intern (concat (prin1-to-string namespace) "-" (prin1-to-string key)))
	(eval value)
	)))
   (seq-partition lst 2)
   ))

(defun get-resource (name)
  "Get X resource value, with a fallback value NAME."
  (let* (
	 (xrdb-fallback-values
	  ;; for when we're away from $HOME.
	  '(
	    ("Emacs.theme"          . "base16-grayscale-light")
	    ("Emacs.powerlinescale" . "1.2")
	    ("st.font"              . "Go Mono-10")
	    ("st.borderpx"          . "15")
	    ("emacs.powerline"      . "bar")
	    ("*.background"         . (face-attribute 'default :background))
	    ))
	 (default (eval (cdr (assoc name xrdb-fallback-values))))
	 )
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
    (if sys/windows?
	(concat "pprint.bat " key)
      (concat "pass " key " 2>/dev/null"))))
  )

(defun reload-init()
  "Reload init.el."
  (interactive)
  (straight-transaction
      (straight-mark-transaction-as-init)
      (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; ref: https://emacs.stackexchange.com/questions/3197/best-way-to-retrieve-values-in-nested-assoc-lists
(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

;; binding wrappers
(defun neeasade/bind (&rest binds)
  (apply 'general-define-key
	 :states '(normal visual)
	 :prefix "SPC"
	 binds))

(defun neeasade/bind-mode(keymaps &rest binds)
  (apply 'general-define-key
	 :prefix "SPC"
	 :states '(visual normal)
	 :keymaps keymaps
	 binds))

(defun neeasade/bind-leader-mode(mode &rest binds)
  (apply 'general-define-key
	 :prefix ","
	 :states '(visual normal)
	 :keymaps (intern (concat (symbol-name mode) "-mode-map"))
	 binds))

(defun nop()
  nil
  )

;; ref https://github.com/energos/dotfiles/blob/master/emacs/init.el#L162
(defun neeasade/install-dashdoc (docset)
  "Install dash DOCSET if dashdocs enabled."
  (if (not sys/windows?)
      (if (boundp 'neeasade-dashdocs)
	  (if (helm-dash-docset-installed-p docset)
	      (message (format "%s docset is already installed!" docset))
	    (progn (message (format "Installing %s docset..." docset))
		   (helm-dash-install-docset (subst-char-in-string ?\s ?_ docset)))
	    )
	)
    )
  )

;; todo: have the above do something like this
;; implies change to  have mode passed/arg diff
;; (defun energos/dash-elisp ()
;; 	(setq-local helm-dash-docsets '("Emacs Lisp")))
;; (add-hook 'emacs-lisp-mode-hook 'energos/dash-elisp)

(defun neeasade/find-or-open (filepath)
  "Find or open FILEPATH."
  (let
      ((filename (file-name-nondirectory filepath)))
    (if (get-buffer filename)
	(counsel-switch-to-buffer-or-window filename)
      (find-file filepath)
      )))

;; todo: this isn't working with anchors in other frames
(defun neeasade/eww-browse-existing-or-new (url)
  "If eww is displayed, use that for URL, else open here."
  (if (get-buffer-window "*eww*" 0)
      (url-retrieve url 'eww-render
		    (list url nil (get-buffer "*eww*")))
    (eww url)
    )
  )
