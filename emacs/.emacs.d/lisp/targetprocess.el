(defun tp-get-story-url ()
  (concat
   "https://civicplus.tpondemand.com/api/v1/userStories/"
   (tp-get-active-userstory)
   "?format=json"
   ;; "?skip=0&take=999&format=json&include=[id,name,linkedTestPlan[id,entityType[id,name],project[id]]]"
   ))

(defun tp-update-git-message ()
  (interactive)
  (use-package request)
  (require 'request)
  (request
   (tp-get-story-url)
   :type "GET"
   :headers '(("Authorization" . (pass "tp_creds")))
   :parser 'json-read
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (let ((result
		      (concat
		       "\n\nFeature #"
		       (number-to-string
			(assoc-recursive data 'Feature 'Id)) " - "
			(assoc-recursive data 'Feature 'Name)
			"\nUser Story #"
			(number-to-string
			 (assoc-recursive data 'Id)) " - "
			 (assoc-recursive data 'Name)
			 "\nhttps://civicplus.tpondemand.com/entity/"
			 (number-to-string (assoc-recursive data 'Id))
			 )))
		 (write-region result nil "~/.git_template")
		 )
	       )))
  )

;; auto get user story id
(defun tp-get-active-userstory ()
  (let (
	(url-read (get-string-from-file "~/.qute_url"))
	)
    (string-match "userstory\/\\([0-9]\\{5\\}\\)" url-read)
    (match-string 1 url-read)
    )
  )
