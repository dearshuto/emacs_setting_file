;; Rust
;; M-x treesit-install-language-grammar が必要
(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :init
  ;; rust-ts-mode がロードされたら eglot を自動起動
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  :config
  ;; 保存時に自動フォーマット (rust-analyzer の機能を利用)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))

;; glsl
(use-package glsl-mode
  :ensure t
  :defer t
  :after package
  :init
  (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.ps\\'" . glsl-mode))
  )

;; lsp
(use-package eglot
  :defer t
  :config
  (add-hook 'before-save-hook 'eglot-format-buffer)
  (add-to-list 'eglot-server-programs '((glsl-mode) "glsl_analyzer"))
  :hook
  ( prog-mode . eglot-ensure)
  :bind
  (
   ("C-." . eglot-code-actions))
  :config
  ;; eglotで無効にする機能
  (setq eglot-ignored-server-capabilities
        '(:inlayHintProvider ;; インラインのヒント表示
          ))
  )


;; eglot が表示するドキュメントをウィンドウ上に表示する設定
(use-package eldoc-box
  :if window-system
  :ensure t
  :defer t
  :after eglot
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  )
