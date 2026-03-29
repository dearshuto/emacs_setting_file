;; ファイルのリロード
(global-auto-revert-mode t)

;; 大文字小文字の区別をしない
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; なめらかスクロール
(setq scroll-conservatively 100
      scroll-margin 5)

;; 80 文字のハイライト
(global-display-fill-column-indicator-mode 1)

;; 画面外の行を折り返さない
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

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

;; --- 便利な小技設定 ---

;; 対応する括弧のハイライト
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; 保存時に行末の余計な空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 最後に改行を自動挿入
(setq require-final-newline t)