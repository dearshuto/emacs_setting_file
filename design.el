;; 全画面化
(toggle-frame-maximized)

;; タブを有効化
(global-tab-line-mode t)

;; 不要な GUI を非表示に設定
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; 半透明化
(set-frame-parameter nil 'alpha 75) ;透明度

;; テーマのロードパスを設定
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'shikama-first)

;; エディタの見た目
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

;; 改行しない
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; 80 文字でハイライト
(global-display-fill-column-indicator-mode 1)

;; 行をハイライト
(global-hl-line-mode t)

;; テーマを追加
;; 設定するのは独自
(leaf doom-themes
  :ensure t
  :require t
  :load-path "~/.emacs.d/themes/"
  :config
  (load-theme 'shikama-first t)
  )

;; モードラインをカスタマイズ
(leaf doom-modeline
  :ensure t
  :require t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 3)
  (doom-modeline-buffer-file-name-style . 'auto)
  (doom-modeline-icon . (display-graphic-p))
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-minor-modes . t)
  (doom-modeline-buffer-state-icon . t)
  (doom-modeline-buffer-modification-icon . t)
  (doom-modeline-buffer-encoding . t)
  (doom-modeline-lsp . t)
  
  )

(leaf neotree
  :ensure t
  :config
  (setq neo-smart-open t)
  (bind-key "C-S-b" 'neotree-toggle)
  (bind-key "RET" 'neotree-enter-hide neotree-mode-map)
  (bind-key "a" 'neotree-hidden-file-toggle neotree-mode-map)
  (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
  (bind-key "<right>" 'neotree-change-root neotree-mode-map)
)
