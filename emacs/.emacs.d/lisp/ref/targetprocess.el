(setq tp-subdomain (pass "tp-subdomain"))

;; debug request '(warn blather)
;; (setq request-log-level 'blather request-message-level 'blather)

(defun tp-get-story-url ()
  (concat
   "https://" tp-subdomain ".tpondemand.com/api/v1/userStories/"
   (tp-get-active-userstory)
   "?format=json"
   ))

(defun tp-update-git-message ()
  (interactive)
  (use-package request)
  (request
   (tp-get-story-url)
   :type "GET"
   :headers `(("Authorization" . ,(pass "tp_creds")))
   :parser 'json-read
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (cl-labels (
			   (tp-get (&rest path) (apply #'assoc-recursive data path))
			   (feature-or-project (&rest item)
			     (if (tp-get 'Feature)
				 (apply #'tp-get 'Feature item)
			       (apply #'tp-get 'Project item)
			       ))

			   (result ()
			     (concat
			      "\n\n" (feature-or-project 'ResourceType) " #"
			      (number-to-string (feature-or-project 'Id)) " - "

			      (feature-or-project 'Name)
			      "\nUser Story #"
			      (number-to-string
			       (tp-get 'Id)) " - "

			       (tp-get 'Name)
			       "\nhttps://" tp-subdomain ".tpondemand.com/entity/"
			       (number-to-string (tp-get 'Id))
			       ))
			   )
		 (write-region (result) nil "~/.git_template")
		 )))))

;; currently active userstory
(defun tp-get-active-userstory ()
  (interactive)
  tp-active-userstory
  )

;; get userstory from saved url
(defun tp-get-userstory-from-url ()
  (let ((url-read (get-string-from-file "~/.qute_url")))
    (string-match "userstory\/\\([0-9]\\{5\\}\\)" url-read)
    (match-string 1 url-read)
    ))

;; set from saved url
(defun tp-set-org-userstory()
  (interactive)
  (org-set-property "targetprocess" (tp-get-userstory-from-url))
  )

(defun tp-set-active()
  (setq tp-active-userstory (org-entry-get nil "targetprocess"))
  (if tp-active-userstory
      (progn
	(org-set-property
	 "url"
	 (concat "https://" tp-subdomain ".tpondemand.com/entity/" tp-active-userstory))
	(tp-update-git-message)
	)))

(advice-add #'neeasade/org-set-active :after #'tp-set-active)
