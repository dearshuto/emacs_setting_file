;; company
(leaf company-mode
  :ensure t
  :emacs>=24.3
  :custom(
	  (company-selection-wrap-around . t)
	  (company-idle-delay . 0)
	  (company-minimum-prefix-length . 1)
	  (completion-ignore-case . t)
	  )
  :bind()
  :global-minor-mode global-company-mode
  )

;; company
(leaf yasnipet
  :ensure t)

;; cargo
(leaf cargo-mode
  :ensure t
  )

;; lsp-mode
(leaf lsp-mode
  :ensure t
  :commands lsp
  :config (local-set-key (kbd "C-.") 'lsp-execute-code-action)
  :config (add-hook 'before-save-hook #'lsp-format-buffer)
  :hook (c++-mode . lsp)
  :hook (c-mode . lsp)
  :hook (glsl-mode . lsp)
  :hook (js-mode . lsp)
  )
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  ;; :config (local-set-key (kbd "C-c") 'lsp-ui-doc-hide)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-delay 0.0416666) ;; 24fps
)
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(leaf rustic
  :ensure t
  :after company
  :after yasnipet)

;; glsl
(leaf glsl-mode
  :ensure t
  )

;; Bat 
(leaf bat-mode
  :ensure t
  )
