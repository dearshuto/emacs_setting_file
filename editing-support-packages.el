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

;; C-@ でマークした場所にカーソルを移動
(global-set-key (kbd "C--") 'pop-global-mark)

;; 外部の変更を自動リロード
(global-auto-revert-mode 1)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-.") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

;; ミニバッファを拡大する設定
(fido-vertical-mode +1)

;; ミニバッファの追加情報
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; あいまい検索
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
