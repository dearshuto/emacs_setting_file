;; -*- lexical-binding: t; -*-

;; Rust
;; M-x treesit-install-language-grammar が必要
(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook
  ;; rust-ts-mode がロードされたら eglot を自動起動
  (rust-ts-mode . eglot-ensure)
  :config
  ;; 保存時に自動フォーマット (rust-analyzer の機能を利用)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))

(use-package c++-ts-mode
  :ensure nil
  :mode
  ("\\.cpp\\'" . c++-ts-mode)
  ("\\.hpp\\'" . c++-ts-mode)
  ("\\.h\\'" . c++-ts-mode)
  :hook
  (c++-ts-mode . eglot-ensure)
  :config
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))

;; C#
(use-package csharp-ts-mode
  :ensure nil
  :mode "\\.cs\\'"
  :hook
  (csharp-ts-mode . eglot-ensure)
  :config
  ;; 保存時に自動フォーマット
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

;; glsl
(use-package glsl-mode
  :ensure t
  :mode
  ("\\.glsl\\'" . glsl-mode)
  ("\\.vs\\'" . glsl-mode)
  ("\\.fs\\'" . glsl-mode)
  :hook
  (glsl-mode . eglot-ensure)
  )

(use-package cmake-ts-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-ts-mode)
          ("\\.cmake\\'" . cmake-ts-mode))
  :hook
  ;; CMake ファイルを開いたときに自動で Eglot（LSP）を起動する
  (cmake-ts-mode . eglot-ensure)
  :config
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)
)

;; lsp
(use-package eglot
  :ensure nil
  :hook
  ;; Eglot 接続時のみ保存時フォーマットを有効化
  (eglot-managed-mode . (lambda ()
                          (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
  :bind
  (:map eglot-mode-map
	("<f2>" . eglot-rename)
        ("C-;" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '((glsl-mode) "glsl_analyzer"))
  (add-to-list 'eglot-server-programs '(cmake-ts-mode . ("neocmakelsp" "stdio")))
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
