(setq tp-subdomain (pass "tp-subdomain"))

;; debug request
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
  (require 'request)
  (request
   (tp-get-story-url)
   :type "GET"
   :headers `(("Authorization" . ,(pass "tp_creds")))
   :parser 'json-read
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (let ((result
		      (concat
		       "\n\nFeature #"
		       (number-to-string (assoc-recursive data 'Feature 'Id)) " - "

		       (assoc-recursive data 'Feature 'Name)
		       "\nUser Story #"
		       (number-to-string
			(assoc-recursive data 'Id)) " - "

			(assoc-recursive data 'Name)
			"\nhttps://" tp-subdomain ".tpondemand.com/entity/"
			(number-to-string (assoc-recursive data 'Id))
		       )))
		 (write-region result nil "~/.git_template")
		 )
	       )))
  )

;; currently active userstory
(defun tp-get-active-userstory ()
  (interactive)
  tp-active-userstory
  )

;; get userstory from saved url
(defun tp-get-userstory-from-url ()
  (let (
  	(url-read (get-string-from-file "~/.qute_url"))
  	)
    (string-match "userstory\/\\([0-9]\\{5\\}\\)" url-read)
    (match-string 1 url-read)
    )
  )

;; set from saved url
(defun tp-set-org-userstory()
  (interactive)
  (org-set-property "targetprocess" (tp-get-userstory-from-url))
  )
