(setq make-backup-files nil)
(setq auto-save-default nil)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; ウィンドウを半透明に設定
(set-frame-parameter nil 'alpha 85)

;; 起動時に全画面化
(push '(fullscreen . maximized) default-frame-alist)

;; 起動画面のカスタマイズ
(use-package dashboard
  :ensure t
  :vc(
      :url "https://github.com/emacs-dashboard/emacs-dashboard.git"
	   :rev :newest)
  :config
  (dashboard-setup-startup-hook))

;; ファイルのリロード
(global-auto-revert-mode t)

;; yes/no -> y/n に変更
(setq use-short-answers t)

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

(use-package lin
  :vc(
      :url "https://github.com/protesilaos/lin.git"
	   :rev :newest)
  :init
  (setq lin-face 'lin-red)
  (lin-global-mode +1))

;; 単語のハイライト
(use-package symbols-overlay
  :vc(
      :url "https://github.com/wolray/symbol-overlay.git"
	   :rev :newest)
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (setq symbol-overlay-idle-time 0.01)
  )

;; カッコのハイライト
(use-package paren
  :config
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  )

(use-package rainbow-delimiters
  :vc(
      :url "https://github.com/Fanael/rainbow-delimiters.git"
	   :rev :newest
	   )
  :hook
  (prog-mode . rainbow-delimiters-mode)
)

;; ファイルツリー
(use-package treemacs
  :vc(
      :url "https://github.com/Alexander-Miller/treemacs.git"
	   :rev :newest)
  :defer t
  :config
  (progn
    (setq treemacs-deferred-git-apply-delay        0.1
	  treemacs-position                        'right
	  
	  ;; ファイル切り替えでツリーも切り替える
	  treemacs-project-follow-mode             t

	  ;; ファイル切り替えを検知してツリーを更新するまでの時間
	  treemacs--project-follow-delay           0.1
	  treemacs-tag-follow-delay                0.1
	  treemacs-file-follow-delay               0.1)
    )
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

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; lsp
(use-package lsp-mode
  :vc(
      :url "https://github.com/emacs-lsp/lsp-mode.git"
	   :rev :newest)
  :init
  (add-hook 'before-save-hook 'lsp-format-buffer)
  :hook
  (c++-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  
  :config
  (setq lsp-completion-provider :none)
  (setq lsp-auto-guess-root t)
  )

(use-package lsp-ui
  :vc(
      :url "https://github.com/emacs-lsp/lsp-ui.git"
	   :rev :newest)
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)

  
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-delay 0.1)
  
  (setq lsp-ui-peek-enable t)
  
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-delay 0.05)
  (setq lsp-ui-sideline-show-code-actions t)
  )

;; 補完
(use-package corfu
   :vc(
       :url "https://github.com/minad/corfu.git"
   	   :rev :newest)
 
   :config
   (setopt corfu-cycle t
   	  corfu-auto t
   	  corfu-auto-delay 0.0
   	  corfu-auto-prefix 1
   	  corfu-on-exact-match 'show
   	  )
   :init
   (global-corfu-mode)
   )

;; モードライン
;; Enable vertico
;; (use-package vertico
;;   :vc(
;;       :url "https://github.com/minad/vertico.git"
;;    	   :rev :newest)
;;   :custom
;;   ;; (vertico-scroll-margin 0) ;; Different scroll margin
;;   (vertico-count 20) ;; Show more candidates
;;   ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
;;   ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
;;   :init
;;   (vertico-mode))

(use-package selectrum
  :vc(
      :url "https://github.com/radian-software/selectrum.git"
	   :rev :newest
	   )
  :init
  (selectrum-mode)
  )

;; アンドゥ
(use-package vundo
  :vc(
      :url "https://github.com/casouri/vundo.git"
	   :rev :newest
	   )
  ;; :config
  ;; (with-eval-after-load 'meow
  ;;   (meow-leader-define-key
  ;;    '("u" . vundo)))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((treemace :url "https://github.com/Alexander-Miller/treemacs.git")
     (lin :url "https://github.com/protesilaos/lin.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
