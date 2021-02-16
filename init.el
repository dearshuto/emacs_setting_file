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
                   '(cursor-color     . "black")  ; カーソルの色
                   '(cursor-type      . box)      ; カーソルの形状
                   )
                  default-frame-alist))

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


;; theme
(download-packages '(doom-themes))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t
	)
  (load-theme 'doom-old-hope t)
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

;; カーソルを淡いグレーに
(set-cursor-color "Cyan")

;; カーソルを点滅させない
(blink-cursor-mode -1)

(global-hl-line-mode t)
(custom-set-faces
 '(hl-line ((t (:background "gray1")))))

;exe path setting
;;NOTE: call after package-initialize
;(when (memq window-system '(mac ns))
;  (exec-path-from-shell-initialize))
;(exec-path-from-shell-initialize)
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
   '(rustic cargo lsp-mode lsp-ui rust-mode company-irony company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; compony
(download-packages '(company))
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

;; exepath
(add-to-list 'exec-path (expand-file-name "/Users/shuto/develop/github/rust/src/tools/rust-analyzer/target/release/"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;; rust
(download-packages '(rustic))
(use-package rustic)


;;(use-package cargo
;;  :ensure t
;;  :hook (rust-mode . cargo-minor-mode)
;;  )

;; lsp
;;(use-package lsp-mode
;;  :ensure t
;;  :hook (rust-mode . lsp)
;;  :bind ("C-c h" . lsp-describe-thing-at-point)
;;  :custom (lsp-rust-server 'rust-analyzer))
;;(use-package lsp-ui
;;  :ensure t)
