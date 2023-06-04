;; make it not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; カラーコードを色付け
(leaf rainbow-mode
  :ensure t
  :leaf-defer t
  :hook
  (web-mode-hook . rainbow-mode))

(leaf yafolding
  :ensure t
  :leaf-defer t
  :hook
  (prog-mode-hook . yafolding-mode))

;;
(add-hook 'rust-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
