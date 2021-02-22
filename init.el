;; 外部パッケージ置き場をロードパスに追加=====================================
(let ((default-directory (locate-user-emacs-file "./externals")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
;;--------------------------------------------------------------------


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
;; カーソルを点滅させない
(blink-cursor-mode -1)

;; 80 文字のハイライト
(global-display-fill-column-indicator-mode 1)

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
  "download packages"
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
		default-frame-alist))
  )
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
  :init (doom-modeline-mode 1))
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-lsp t)


;; eshel で Ctrl+l したときに全クリアされる設定
(defun eshell-clear-buffer ()
  "Clear terminal"
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-glsl company-glsl rainbow-mode rainbow-delimiters smooth-scroll highlight-parentheses rustic cargo lsp-mode lsp-ui rust-mode company-irony company)))

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

;; 同じ単語をハイライト
(use-package symbol-overlay
  :ensure t
  :config
  (setq symbol-overlay-idle-time 0.01)
  (add-hook 'prog-mode-hook 'symbol-overlay-mode)
  (add-hook 'emacs-lisp-mode 'symbol-overlay-mode)
  )
  
;; -------------------------------------------------------------------

;; irony
(download-packages '(irony))

;; compony
(download-packages '(company))
(download-packages '(company-irony))
(add-hook 'after-init-hook 'global-company-mode)
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;(add-to-list 'company-backends 'company-irony) ; backend追加
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(with-eval-after-load 'company
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-auto-expand t) ;; 1個目を自動的に補完
  (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
  
  (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
  )

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-idle-change-delay 0.1)
  )

;; exepath
(add-to-list 'exec-path (expand-file-name "/Users/shuto/develop/github/rust/src/tools/rust-analyzer/target/release/"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(add-to-list 'exec-path (expand-file-name "/usr/local/bin/"))

;; rust
(download-packages '(rustic))
(use-package rustic
  :ensure t
  :config
  ;; rustfmt を rustic 経由で走らせるとバグるので、外部プロセスとして走らせる
  ;;  (setq-default rustic-format-trigger 'on-save)
  (defun execute-rustfmt ()
    (interactive)
    (call-process "rustfmt" nil t nil buffer-file-name))

  ;; キーバインドがうまくいかない。とりあえず M-x で呼び出す
;;    (define-key rustic-mode-map "\C-x\C-x" 'my-pwd)
  )


;;(use-package cargo
;;  :ensure t
;;  :hook (rust-mode . cargo-minor-mode)
;;  )

;; lsp
(download-packages '(lsp-mode))
;;(use-package lsp-mode
;;  :ensure t
;;  :hook (rust-mode . lsp)
;;  :bind ("C-c h" . lsp-describe-thing-at-point)
;;  :custom (lsp-rust-server 'rust-analyzer))
;;(use-package lsp-ui
;;  :ensure t)
(put 'erase-buffer 'disabled nil)

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
