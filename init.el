(when (not (equal system-type 'windows-nt))
  (when (require 'exec-path-from-shell nil t)
    (exec-path-from-shell-initialize)))

(require 'package)

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(package-install 'use-package)
(package-install 'leaf)
(package-install 'doom-themes)

;; グローバルなキーバインド
(load-file "~/.emacs.d/key-mapping.el")

;; エディタの見た目に関する設定
(load-file "~/.emacs.d/design.el")

;; 編集補助パッケージの設定
(load-file "~/.emacs.d/editing-support-packages.el")

;; 言語ごとの設定
(load-file "~/.emacs.d/language-settings.el")

(require 'lsp-mode)

;; eglot を ON にする mode を指定
;;(add-hook 'glsl-mode-hook 'lsp-mode)
;; (add-to-list 'eglot-server-programs
;; 	     `(glsl-mode . ("/Users/shuto/develop/github/glsl-language-server/build/glslls" "--port" :autoport))
;; 	     )

;; ivy
(leaf ivy
  :ensure t
  :config
  (setq ivy-height 5))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7d16bd8e750eb21e05094123a98ff48c6bd2de12c16482b88e71cce3e857885c" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" default))
 '(package-selected-packages
   '(company yasnippet which-key use-package undo-tree symbol-overlay rustic rainbow-mode rainbow-delimiters lsp-ui leaf json-mode flycheck exec-path-from-shell eglot doom-themes doom-modeline dap-mode csproj-mode csharp-mode counsel company-lua company-glsl company-box cmake-mode cargo))
 '(warning-suppress-log-types '((leaf) (leaf) (leaf)))
 '(warning-suppress-types '((leaf) (leaf))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
