;; -*- mode: emacs-lisp -*-

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; 3rd party/other
(load-from "external" "le-eval-and-insert-results")
(require 'le-eval-and-insert-results)
