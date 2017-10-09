;; -*- mode: emacs-lisp -*-
;; ref: https://github.com/syl20bnr/spacemacs/blob/master/core/templates/.spacemacs.template

(load-from "lisp" "layers")

(defun dotspacemacs/layers ()
  (load-settings
   "dotspacemacs"
   '(
     distribution 'spacemacs
     enable-lazy-installation 'unused
     ask-for-lazy-installation t
     configuration-layer-path '()
     frozen-packages '()
     excluded-packages '()
     install-packages 'used-only
     additional-packages
      '(
        base16-theme
        editorconfig
        helm-dash
        zoom
                    )

      configuration-layers
                  (append
                   spacemacs/layers/core
                   spacemacs/layers/extra
                   spacemacs/layers/langs
                   )
        )))

(defun dotspacemacs/init ()
  (load-settings
   "dotspacemacs"
   '(
     startup-banner nil
     elpa-https t
     line-numbers nil
     elpa-timeout 5
     check-for-update nil
     elpa-subdirectory nil
     editing-style 'hybrid
     verbose-loading nil
     startup-lists '((recents . 5)
                     (projects . 7))

     startup-buffer-responsive t
     scratch-mode 'elisp-mode
     colorize-cursor-according-to-state nil

     themes (list (intern (get-resource "Emacs.theme")))

     default-font (list
                   (get-resource "st.font")
                   :weight 'normal
                   :width 'normal
                   :powerline-scale (string-to-number (get-resource "Emacs.powerlinescale")))

     leader-key "SPC"
     emacs-command-key "SPC"
     ex-command-key ":"
     emacs-leader-key "M-m"
     major-mode-leader-key ","
     major-mode-emacs-leader-key "C-M-m"
     distinguish-gui-tab nil
     remap-Y-to-y$ t
     retain-visual-state-on-shift t
     visual-line-move-text nil
     ex-substitute-global nil
     default-layout-name "Default"
     auto-generate-layout-names nil
     display-default-layout nil
     auto-resume-layouts t
     large-file-size 1
     auto-save-file-location 'cache
     max-rollback-slots 5
     helm-resize nil
     helm-no-header t
     helm-position 'top
     helm-use-fuzzy 'always
     enable-paste-transient-state nil
     which-key-delay 0.4
     which-key-position 'right-then-bottom
     loading-progress-bar t
     fullscreen-at-startup nil
     fullscreen-use-non-native nil
     maximized-at-startup nil
     active-transparency 90
     inactive-transparency 90
     show-transient-state-title t
     show-transient-state-color-guide t
     mode-line-unicode-symbols nil
     smooth-scrolling t
     folding-method 'evil
     smartparens-strict-mode nil
     smart-closing-parenthesis t
     highlight-delimiters 'all
     persistent-server nil
     search-tools '("ag" "pt" "ack" "grep")
     default-package-repository nil
     frame-title-format "%b - %I"
     icon-title-format nil
     whitespace-cleanup 'trailing
     pretty-docs t
     zone-out-when-idle nil
     )))

(defun dotspacemacs/user-init ()
  (defconst custom-file (expand-file-name ".spacemacs.custom.el" user-home-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file)
  )
