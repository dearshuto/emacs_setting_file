;; ファイルのリロード
(global-auto-revert-mode t)

;; Linux のフォント設定
(when (equal system-type 'gnu/linux)
  (add-to-list 'default-frame-alist
               '(font . "UbuntuMono-11")))

;; Mac のフォント設定
(when (equal system-type 'darwin)
  (add-to-list 'default-frame-alist
               '(font . "Menlo-11")))


;; 不要な GUI を無効化
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

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

;; カーソルを点滅させない
(blink-cursor-mode -1)

;; 80 文字のハイライト
(global-display-fill-column-indicator-mode 1)

;; 横方向のハイライト
(global-hl-line-mode t)

;; 画面外の行を折り返さない
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; 行番号を表示
(global-display-line-numbers-mode t) 

;; MELPA をパッケージ管理に追加
(use-package package
  :init
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  :config
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  )

;; なめらかスクロール
(setq scroll-conservatively 100
      scroll-margin 5)

;; 起動画面のカスタマイズ
(use-package dashboard
  :ensure t
  :after package
  :config
  (dashboard-setup-startup-hook))

;; マルチカーソル
(use-package multiple-cursors
  :ensure t
  :bind
  (:map global-map(
		   ;; 単語を選択していく
		   ;; 単語を洗濯していない状態だと次の行にカーソルを追加する挙動になる
		   ("C->" . mc/mark-next-like-this)
		   ("C-<" . mc/mark-previous-like-this))
	)
  )

;; 補完強化
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  )

;; 曖昧検索
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; orderless を corfu のあいまい検索に使う設定
  (with-eval-after-load 'corfu
      (add-hook 'corfu-mode-hook
                (lambda ()
                  (setq-local orderless-matching-styles '(orderless-flex)))))
  ;; orderless を company のあいまい検索に使う設定
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook
              (lambda ()
                (setq-local orderless-matching-styles '(orderless-flex)))))
  )

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

;; モードライン
(use-package vertico
  :ensure t
  :after package
  :init
  (vertico-mode)
    :custom
    (vertico-scroll-margin 0) ;; Different scroll margin
    (vertico-count 5) ;; Show more candidates
    (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
    (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
    )

;; Rust
(use-package rust-mode
  :ensure t
  :after package
  )

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

;; Company
(use-package company
  ;; GUI のときは corfu を使う
  :if (not window-system)
  :ensure t
  :after package
  :hook (prog-mode . company-mode)
  :config
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

;; company のアイコン対応
(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  )

;; corfu
(use-package corfu
  ;; emacs 30 の CLI では corfu がウィンドウを出せないので company を使う
  ;; emacs 31 が使えるようになったら corfu に一本化したい
  :if window-system
  :ensure t
  :after package
  :init
  (global-corfu-mode)
  :config
  (setopt corfu-cycle t
   	  corfu-auto t
   	  corfu-auto-delay 0.0
   	  corfu-auto-prefix 1
   	  corfu-on-exact-match 'show
   	  )
  )

;; corfu の補完にアイコンをつける
(use-package kind-icon
  :ensure t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; lsp
(use-package eglot
  :defer t
  :config
  (add-hook 'before-save-hook 'eglot-format-buffer)
  (add-to-list 'eglot-server-programs '((rust-mode) "rust-analyzer"))
  (add-to-list 'eglot-server-programs '((glsl-mode) "glsl_analyzer"))
  :hook
  ( prog-mode . eglot-ensure)
  :bind
  (
   ("C-." . eglot-code-actions))
  :config
  ;; eglotで無効にする機能
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider ;; カーソル下のシンボルハイライト、無効化すると軽量になる
          :inlayHintProvider ;; インラインのヒント表示
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
