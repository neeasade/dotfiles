;; -*- mode: emacs-lisp -*-

(defun neeasade/style ()
  (dotspacemacs/init)
  (spacemacs/load-theme (car dotspacemacs-themes))
  (spacemacs/set-default-font dotspacemacs-default-font)

  (setq powerline-default-separator (get-resource "emacs.powerline"))

  ; sync modeline background color?
  ;(set-face-attribute
   ;'spacemacs-normal-face nil :inherit 'mode-line)

  (set-face-background 'font-lock-comment-face nil)

  ; todo: make this on all frames, not just current
  (set-frame-parameter (selected-frame) 'internal-border-width
                       (string-to-number (get-resource "st.borderpx")))

  ; sync w/ term background
  (set-background-color
   (get-resource "*.background"))

  ; assume softer vertical border by matching comment face
  (set-face-attribute 'vertical-border
                      nil
                      :foreground (face-attribute 'font-lock-comment-face :foreground))

  ; this doesn't persist across new frames even though the docs say it should
  (set-face-attribute 'fringe nil :background nil)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-face-attribute 'fringe nil :background nil)
              )
            )

  ; set font on current and future
  (set-face-attribute 'default nil :font (get-resource "st.font"))
  (set-frame-font (get-resource "st.font") nil t)

  ; NO BOLD (set-face-bold-p doesn't cover everything, some fonts use slant and underline as bold...)
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :weight 'normal
                              :slant 'normal
                              :underline nil
                              ;:inherit nil
                              ))
        (face-list))

  ; TODO make emms a bit prettier
  ;(face-attribute 'font-lock-comment-face :foreground)
  ;(set-face-attribute 'emms-browser  nil :background nil)
  ;; emms-metaplaylist-mode-current-face
  ;; emms-metaplaylist-mode-face
  ;; emms-browser-track-face
  ;; emms-browser-album-face
  ;; emms-browser-performer-face
  ;; emms-browser-composer-face
  ;; emms-browser-artist-face
  ;; emms-browser-year/genre-face
  ;; emms-stream-url-face
  ;; emms-stream-name-face
  ;; emms-playlist-selected-face
  ;; emms-playlist-track-face


  ; done at end so it has correct font reference
  (spaceline-compile)
  )
