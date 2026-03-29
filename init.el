;;; --- パッケージ管理の初期化 ---
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; --- 設定ファイルの読み込み用関数 ---
(defun load-conf (file)
  (load (expand-file-name (concat "config/" file) user-emacs-directory)))

;; 各設定をロード
(load-conf "init-ui.el")
(load-conf "init-editing.el")
(load-conf "init-completion.el")
(load-conf "init-language.el")