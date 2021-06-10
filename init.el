;;; init.el --- Setting File ---

;;; Commentary:
;; of Shuto, by Shuto, for Shuto

;;; Code:

;; 外部パッケージ置き場をロードパスに追加=====================================
(let ((default-directory (locate-user-emacs-file "./externals")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
;;--------------------------------------------------------------------

;; 環境変数持ち込む
(when (not (equal system-type 'windows-nt))
  (when (require 'exec-path-from-shell nil t)
	(exec-path-from-shell-initialize)))

;; make it not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
;; --------------------------------------------------------------------------------

;; ウィンドウを半透明にする設定
;; Color
(if window-system (progn
    (set-frame-parameter nil 'alpha 75) ;透明度
    ))
;; --------------------------------------------------------------------------------


;; GUI
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; 全画面化
(toggle-frame-maximized)

;; ファイルのリロード
(global-auto-revert-mode t)

;; ウィンドウの見た目に関する設定

; ウィンドウを縦に2分割する設定
;;(split-window-horizontally (/ (frame-width) 2))

; ウィンドウのスタイルの設定
(setq default-frame-alist
          (append (list
                   '(foreground-color . "white")  ; 文字色
                   '(background-color . "black")  ; 背景色
                   '(border-color     . "white")  ; ボーダー色
                   '(mouse-color      . "black")  ; マウスカーソルの色
                   '(cursor-color     . "cyan")  ; カーソルの色
                   '(cursor-type      . bar)      ; カーソルの形状
                   )
                  default-frame-alist))

;; タブを有効化
(global-tab-line-mode t)

;; カーソルを点滅させない
(blink-cursor-mode -1)

;; no return
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; 80 文字のハイライト
(global-display-fill-column-indicator-mode 1)

;; Ctrl+矢印でウィンドウを移動するキーバインド
(global-unset-key (kbd "S-<up>"))
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;; Ctrl+L で現在の行を削除するショートカットを設定
(global-set-key (kbd "C-l") 'kill-whole-line)

;; バッファを閉じるキーバインド
(global-set-key (kbd "C-q") 'kill-this-buffer)

;; アンドゥ
(global-set-key (kbd "C-z") 'undo)
;; --------------------------------------------------------------------------------


;; Emacsに付属しているパッケージ管理システムを拡張する設定
(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
       ("melpa" . "http://melpa.org/packages/")
       ("org" . "http://orgmode.org/elpa/")))

;; パッケージをダウンロードする関数
(defun download-packages (package-names)
  "Download packages."
  (dolist (package-name package-names)
    (unless (package-installed-p package-name)
      (package-refresh-contents)
      (package-install package-name))))

(download-packages '(use-package))

;; theme
(download-packages '(doom-themes))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(use-package doom-themes
  :load-path "./themes"
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'shikama-first t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  )


;; mac のフォント設定
(when (equal system-type 'darwin)
  (setq default-frame-alist
		(append (list
				 '(font . "Menlo-11"))
				default-frame-alist)))

;; Windows のフォント
(when (equal system-type 'windows-nt)
  (setq default-frame-alist
		(append (list
				 '(font . "Consolas-11"))
				default-frame-alist)))

;;(setq default-frame-alist
;;      (append (list
;;              '(font . "Menlo-11"))
;;              default-frame-alist))

(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray1")))))


;; mode line のカスタマイズ
(download-packages '(doom-modeline))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-lsp t)
  )

;; eshel で Ctrl+l したときに全クリアされる設定
(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;exe path setting
;;NOTE: call after package-initialize
;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))
;;(exec-path-from-shell-initialize)
;; --------------------------------------------------------------------------------


;; yatex
;; EmacsをTeXエディタにするためのプラグイン
;; load yatex-mode
;(setq auto-mode-alist
;(cons (cons "\.tex$" 'yatex-mode) auto-mode-alist))
;(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; --------------------------------------------------------------------------------


;; 文章作成時の漢字コードの設定
					; 1 = Shift_JIS, 2 = ISO-2022-JP, 3 = EUC-JP, 4 = UTF-8
					; コードを指定してしまうと，別のコードのファイルも勝手に変換される
					; ここで指定したコードに変換されてしまいトラブルのもとに
					; なるので，nilにしておくのが吉。
;(setq YaTeX-kanji-code nil)
;; --------------------------------------------------------------------------------



;; exec-path-from-shell
;; bashのパスをemaceに引き継がせるプラグイン.
;(exec-path-from-shell-initialize)~

;; モードライン ---------------------------------------------------------
;; ivy
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 5) ;; minibufferのサイズを拡大！（重要）
  (setq ivy-extra-directories nil))

;; counsel
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../"))))

;; Suggestion
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-bottom) ; 下に表示
  (which-key-mode t)
  (setq which-key-idle-delay 0.01)
  (setq which-key-idle-secondary-delay 0.01)
  (setq which-key-side-window-max-height 5))

;; --------------------------------------------------------------------


;; 編集補佐 ------------------------------------------------------------

;; Ctrl + Shift + Z でアンドゥ
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :bind (("C-S-z" . undo-tree-redo))
  )

;; 同じ単語をハイライト
(use-package symbol-overlay
  :ensure t
  :config
  (setq symbol-overlay-idle-time 0.01)
  (add-hook 'prog-mode-hook 'symbol-overlay-mode)
  (add-hook 'emacs-lisp-mode 'symbol-overlay-mode)
  )

;; 対応するカッコを自動挿入
(electric-pair-mode 1)

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
;; -------------------------------------------------------------------

;; compony
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
 ;; (setq company-backends "(company-capf)")
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  :bind (
	 :map company-active-map
	      ("<tab>" . company-complete-selection) ;; TABで候補を設定
	      ("C-S-h" . company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next)
	 )
  )

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-idle-change-delay 0.1)
  )

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
  (add-hook 'rustic-mode-hook 'eglot-ensure)
  )

;; exepath
(add-to-list 'exec-path (expand-file-name "/Users/shuto/develop/github/rust/src/tools/rust-analyzer/target/release/"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(add-to-list 'exec-path (expand-file-name "/usr/local/bin/"))

;; rust
(use-package rustic
  :ensure t
  :defer t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  ;; Rustfmt を rustic 経由で走らせるとバグるので、外部プロセスとして走らせる
  ;;  (setq-default rustic-format-trigger 'on-save)
  (defun execute-rustfmt ()
    (interactive)
    (call-process "rustfmt" nil t nil buffer-file-name))

  ;; Format on Save
  (setq-default rustic-format-trigger 'on-save)
  (setq rustic-rustfmt-bin "~/.cargo/bin/rustfmt")
  )


;; cargo
(use-package cargo
  :ensure t
  :bind (("M-b" . cargo-process-build)))

;; lsp ---------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :config
  ;; debug
  (setq lsp-log-io nil)
  ;; general
  (setq lsp-auto-guess-root t)
  (setq lsp-response-timeout 5)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-peek-enable t)
  )
(put 'erase-buffer 'disabled nil)
;; -------------------------------------------------------------------

(when (not (equal system-type 'windows-nt))
	  (use-package exec-path-from-shell
		:ensure
		:init (exec-path-from-shell-initialize))
	  )

;; dap
(use-package dap-mode
  :ensure t
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-lldb)
  (require 'dap-codelldb)  
  (dap-codelldb-setup)
  (dap-register-debug-template "Rust::CODELLDB"
			     (list :type "codelldb"
				   :request "launch"
				   :name "CODELLDB::Run"
				   :gdbpath "rust-lldb"
				   :target nil
				   :cwd nil
				   ))
  )
;; ----

;; C++ ---------------------------------------------------------------
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook #'lsp)
;;--------------------------------------------------------------------

;; GLSL --------------------------------------------------------------
(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
  )
(use-package company-glsl
  :ensure t
  :config
  (add-to-list 'company-backends 'company-glsl)
  (add-hook 'company-glsl 'glsl-mode)
  )

;; flycheck はパーケージシステムにないので :ensure はなし
(use-package flycheck-glsl)
;; -------------------------------------------------------------------


;; cmake -------------------------------------------------------------
(use-package cmake-mode
    :ensure t)
;; -------------------------------------------------------------------


;; lua ---------------------------------------------------------------
(use-package lua-mode
  :ensure t)
(use-package company-lua
  :ensure t
  :config
  (defun my-lua-mode-company-init ()
    (setq-local company-backends '((company-lua
                                    company-etags
                                    company-dabbrev-code
                                    company-yasnippet))))
  (add-hook 'lua-mode-hook #'my-lua-mode-company-init)
  )
;; -------------------------------------------------------------------


;; json --------------------------------------------------------------
(use-package json-mode
  :ensure t)
;; -------------------------------------------------------------------

;; カラーコードを可視化
(download-packages '(rainbow-mode))
(use-package rainbow-mode
  :config
  (setq rainbow-html-colors t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  )

;; カッコのハイライト
(show-paren-mode t)
(require 'paren)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match "#4dccff")
(set-face-foreground 'show-paren-match "#020266")
(set-face-underline 'show-paren-match "#ffff00")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; 連続したカッコを七色に塗り分ける
(download-packages '(rainbow-delimiters))
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 保存時にモードラインを光らせる
(add-hook 'after-save-hook
	  (lambda ()
            (let ((orig-fg (face-background 'mode-line)))
              (set-face-background 'mode-line "#009900")
              (run-with-idle-timer 0.25 nil
				   (lambda (fg) (set-face-background 'mode-line fg))
				   orig-fg))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rust-analyzer which-key use-package symbol-overlay rustic rainbow-mode rainbow-delimiters lsp-ui json-mode flycheck exec-path-from-shell eglot doom-themes doom-modeline dap-mode counsel company-lua company-glsl cmake-mode cargo)))
