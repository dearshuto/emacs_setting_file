;; 不要な GUI を早期に無効化して起動を高速化
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; パッケージの初期化を init.el で手動で行うため抑制
(setq package-enable-at-startup nil)