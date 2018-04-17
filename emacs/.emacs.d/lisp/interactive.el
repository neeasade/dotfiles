;;; interactive.el --- some interactive functions
;;; commentary:
;;; code:

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun what-major-mode ()
  "Reveal current major mode."
  (interactive)
  (message "%s" major-mode))

(defun what-minor-modes ()
  (interactive)
  (message
   (format "%s"
	   (delq nil
		 (mapcar
		  (lambda (x)
		    (let ((car-x (car x)))
		      (when (and (symbolp car-x) (symbol-value car-x))
			x)))
		  minor-mode-alist))
	   ))
  )

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

(defun neeasade/get-functions()
  (mapcar*
   (lambda(item)
     (s-chomp (s-chop-prefix "defun neeasade/" (car item))))
   (s-match-strings-all
    "defun neeasade/[^ \(\)]+"
    (get-string-from-file "~/.emacs.d/lisp/theworld.el"))
   )
  )

(defun neeasade/jump-config()
  (interactive)
  (ivy-read
   "(): " (neeasade/get-functions)
   :action
   (lambda (option)
     (find-file "~/.emacs.d/lisp/theworld.el")
     (goto-char (point-min))
     (re-search-forward (concat "neeasade/" option))
     ))
  )

(defun neeasade/toggle-bloat()
  (interactive)
  (if (not (bound-and-true-p company-mode))
      (progn
	(company-mode)
	(flycheck-mode)
	(font-lock-mode)
	)
    (progn
      (company-mode -1)
      (flycheck-mode -1)
      (font-lock-mode 0)
      )
    )
  )

(defun neeasade/toggle-bloat-global()
  (interactive)
  (if (not (bound-and-true-p global-company-mode))
      (progn
	(global-company-mode -1)
	(global-flycheck-mode -1)
	(global-font-lock-mode 0)
	)
    (progn
	(global-company-mode 1)
	(global-flycheck-mode 1)
	(global-font-lock-mode 1)
      )
    )
  )


(neeasade/bind
 ;; reconsider these, moved from w -> q for query
 "qf" 'what-face
 "qm" 'what-major-mode
 "qi" 'what-minor-modes

 "fE" 'sudo-edit
 "jc" 'neeasade/jump-config
 "tb" 'neeasade/toggle-bloat
 )


(provide 'interactive)
;;; interactive.el ends here

