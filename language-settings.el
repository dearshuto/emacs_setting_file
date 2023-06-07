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

;; rustic
(leaf lsp-mode
  :ensure t
  :require t
  :commands lsp
  )
(leaf rustic
  :ensure t
  :after company
  :after yasnipet)

;; glsl
(leaf glsl-mode
  :ensure t
  )

;; Bat 
(leaf bats-mode
  :ensure t
  )
