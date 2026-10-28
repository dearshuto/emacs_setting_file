;; -*- lexical-binding: t; -*-

;; Linux のフォント設定
(when (equal system-type 'gnu/linux)
  (add-to-list 'default-frame-alist
               '(font . "UbuntuMono-11")))

;; Mac のフォント設定
(when (equal system-type 'darwin)
  (add-to-list 'default-frame-alist
               '(font . "Menlo-11")))

;; ウィンドウを半透明に設定
(set-frame-parameter nil 'alpha 67)

;; 起動時に全画面化
(push '(fullscreen . maximized) default-frame-alist)

;; ウィンドウスタイルの設定
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

;; タブの有効化
(global-tab-line-mode t)

;;  モダン右クリックメニュー
(context-menu-mode t)

;; カーソルを点滅させない
(blink-cursor-mode -1)

;; 横方向のハイライト
(global-hl-line-mode t)

;; 行番号を表示
(global-display-line-numbers-mode t)
;; 行番号を常時表示する（絶対行番号）
(line-number-mode 1)
;; 列番号（カーソルが何文字目にあるか）を常時表示する
(column-number-mode 1)

;; 起動画面のカスタマイズ
(use-package dashboard
  :ensure t
  :after package
  :config
  (dashboard-setup-startup-hook))

;; ファイルツリー
(use-package treemacs
  :ensure t
  :after package
  :defer t
  :config
  ( setq treemacs-deferred-git-apply-delay        0.1)
  ( setq treemacs-position                        'right)
  
  ;; ファイル切り替えでツリーも切り替える
  (treemacs-project-follow-mode)
	  
  ;; ファイル切り替えを検知してツリーを更新するまでの時間
  ( setq treemacs--project-follow-delay           0.1)
  ( setq treemacs-tag-follow-delay                0.1)
  ( setq treemacs-file-follow-delay               0.1)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("M-b"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  )

;; treemacs で表示したファイルツリーをダブルクリックではなくシングルクリックで操作できるようにする
(with-eval-after-load 'treemacs
 (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package doom-themes
  :ensure t)

;; モードラインの表示をリッチにする
(use-package doom-modeline
  :ensure t
  ;; :init だとエラーになった。確実に初期化後に有効化するために hook を使う
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding t)
  (doom-modeline-lsp t)
  )
;; テーマのロード設定
(add-to-list 'custom-theme-load-path
	     (expand-file-name "themes" user-emacs-directory))

(load-theme 'shikama-first t)

;; tree-sitter がない場合の置き場所
(setq treesit-extra-load-path
      (list (expand-file-name "grammars" user-emacs-directory)))
