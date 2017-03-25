;; make it not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
;; --------------------------------------------------------------------------------


;; ウィンドウを半透明にする設定
;; Color
(if window-system (progn
    (set-frame-parameter nil 'alpha 50) ;透明度
    ))
;; --------------------------------------------------------------------------------


;; ウィンドウの見た目に関する設定

; ウィンドウを縦に2分割する設定
(split-window-horizontally (/ (frame-width) 2))

; ウィンドウのスタイルの設定
(setq default-frame-alist
          (append (list
                   '(foreground-color . "white")  ; 文字色
                   '(background-color . "black")  ; 背景色
                   '(border-color     . "white")  ; ボーダー色
                   '(mouse-color      . "black")  ; マウスカーソルの色
                   '(cursor-color     . "black")  ; カーソルの色
                   '(cursor-type      . box)      ; カーソルの形状
                   '(top . 5) ; ウィンドウの表示位置（Y座標）
                   '(left . 50) ; ウィンドウの表示位置（X座標）
                   '(width . 150) ; ウィンドウの幅（文字数）
                   '(height . 95) ; ウィンドウの高さ（文字数）
                   )
                  default-frame-alist))
;; --------------------------------------------------------------------------------


;; Emacsに付属しているパッケージ管理システムを拡張する設定
;; add package lists
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; 初期化
(package-initialize)
;;exe path setting
;;NOTE: call after package-initialize
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-initialize)
;; --------------------------------------------------------------------------------


;; yatex
;; EmacsをTeXエディタにするためのプラグイン
;; load yatex-mode
(setq auto-mode-alist 
      (cons (cons "\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; --------------------------------------------------------------------------------


;; 文章作成時の漢字コードの設定
; 1 = Shift_JIS, 2 = ISO-2022-JP, 3 = EUC-JP, 4 = UTF-8
; コードを指定してしまうと，別のコードのファイルも勝手に変換される
; ここで指定したコードに変換されてしまいトラブルのもとに
; なるので，nilにしておくのが吉。
(setq YaTeX-kanji-code nil)
;; --------------------------------------------------------------------------------


;; exec-path-from-shell
;; bashのパスをemaceに引き継がせるプラグイン.
(exec-path-from-shell-initialize)
