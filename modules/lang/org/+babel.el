;;; lang/org/+babel.el -*- lexical-binding: t; -*-

(defvar +org-babel-languages
  '(browser
    calc
    css
    emacs-lisp
    js
    latex
    ledger
    lisp
    plantuml
    python
    restclient ; ob-restclient
    rust       ; ob-rust
    shell
    sqlite
    sql-mode   ; ob-sql-mode
    translate
    typescript) ; ob-translate
  "A list of org-babel languages to load.")


(after! org
  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil) ; you don't need my permission

  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-loop for sym in +org-babel-languages
            collect (cons sym t)))
  (add-to-list 'org-structure-template-alist
               '("el"  "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
  (add-to-list 'org-structure-template-alist
               '("pg"  "#+BEGIN_SRC sql :engine postgresql\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
  (add-to-list 'org-structure-template-alist
               '("js"  "#+BEGIN_SRC js\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
  ;; I prefer C-c C-c for confirming over the default C-c '
  (map! :map org-src-mode-map "C-c C-c" #'org-edit-src-exit)

  ;; In a recent update, `org-babel-get-header' was removed from org-mode, which
  ;; is something a fair number of babel plugins use. So until those plugins
  ;; update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p)))
