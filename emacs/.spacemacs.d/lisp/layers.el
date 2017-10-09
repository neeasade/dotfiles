(defvar spacemacs/layers/core
  '(
    (shell
     :variables
     shell-default-height 40
     shell-default-position 'top)

    evil-commentary

    better-defaults helm ranger

    colors
    syntax-checking

    (spell-checking
     ;; TODO: add a check here for aspell and make decision based on that
     :variables
     spell-checking-enable-by-default nil)

    (auto-completion
     :variables
     auto-completion-tab-key-behavior 'complete
     auto-completion-complete-with-key-sequence-delay 0
     auto-completion-enable-help-tooltip t
     company-quickhelp-delay 0.1)

    themes-megapack
    git version-control
    pdf-tools
    mu4e
    dash

    (org
     :variables
     org-want-todo-bindings t)
    ))

(defvar spacemacs/layers/extra
  '(
    ))

(defvar spacemacs/layers/langs
  '(
    c-c++
    clojure
    emacs-lisp
    html
    markdown
    nixos

    javascript
    (typescript
     :variables
     typescript-fmt-on-save t)

    latex
    python
    vimscript
    windows-scripts
    yaml
    ))
