;; Ctrl+Shift+R で設定ファイルをリロード
(global-set-key (kbd "C-S-r") 'eval-buffer)

;; Ctrl+L で現在の行を削除するショートカット
(global-set-key (kbd "C-l") 'kill-whole-line)

;; 単語選択
(global-set-key (kbd "C-S-w") 'mark-word)

;; アンドゥ、リドゥ
(global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "C-S-z") 'redo)

;; バッファを閉じるキーバインド
(global-set-key (kbd "C-q") 'kill-this-buffer)
