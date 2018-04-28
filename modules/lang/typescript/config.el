;;; lang/typescript/config.el -*- lexical-binding: t; -*-
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-tsserver-executable  "/Users/baljeetkumar/.nvm/versions/node/v8.9.4/lib/node_modules/typescript/bin/tsserver")
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(def-package! typescript-mode
  :mode "\\.ts$"
  :config
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'ycmd-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (set! :electric 'typescript-mode :chars '(?\} ?\)) :words '("||" "&&"))

  ;; TODO tide-jump-back
  ;; TODO (tide-jump-to-definition t)
  ;; TODO convert into keybinds
  ;; (set! :emr 'typescript-mode
  ;;       '(tide-find-references             "find usages")
  ;;       '(tide-rename-symbol               "rename symbol")
  ;;       '(tide-jump-to-definition          "jump to definition")
  ;;       '(tide-documentation-at-point      "current type documentation")
  ;;       '(tide-restart-server              "restart tide server"))
  )


(def-package! tide
  :after typescript-mode
  :config
  (set! :company-backend 'typescript-mode '(company-tide))
  (set! :jump 'typescript-mode
    :definition #'tide-jump-to-definition
    :references #'tide-references
    :documentation #'tide-documentation-at-point)

  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil))

  (defun +typescript|init-tide ()
    (when (or (eq major-mode 'typescript-mode)
              (and (eq major-mode 'web-mode)
                   buffer-file-name
                   (equal (file-name-extension buffer-file-name) "tsx")))
      (tide-setup)
      (flycheck-mode +1)
      (eldoc-mode +1)
      (setq tide-project-root (doom-project-root))))
  (add-hook! (typescript-mode web-mode) #'+typescript|init-tide))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
; /Users/baljeetkumar/.nvm/versions/node/v8.9.4/lib/node_modules/typescript/bin/tsserver
;; formats the buffer before saving
