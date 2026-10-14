;;; --- パッケージ管理の初期化 ---
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; インストールされていないパッケージがある場合、アーカイブを自動更新してからインストールする
(setq use-package-always-ensure t)
(unless package-archive-contents
  (package-refresh-contents))

;; Custom による自動追記先を別ファイル（custom.el）に変更
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; もし custom.el が存在していれば読み込む
(load custom-file 'noerror)

;;; --- 設定ファイルの読み込み用関数 ---
(defun load-conf (file)
  (load (expand-file-name (concat "config/" file) user-emacs-directory)))

;; 各設定をロード
(load-conf "init-ui.el")
(load-conf "init-editing.el")
(load-conf "init-completion.el")
(load-conf "init-language.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
