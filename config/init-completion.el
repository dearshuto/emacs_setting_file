;; モードライン
(use-package vertico
  :ensure t
  :after package
  :init
  (vertico-mode)
    :custom
    (vertico-scroll-margin 0) ;; Different scroll margin
    (vertico-count 5) ;; Show more candidates
    (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
    (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
    )

;; 曖昧検索
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; orderless を corfu のあいまい検索に使う設定
  (with-eval-after-load 'corfu
      (add-hook 'corfu-mode-hook
                (lambda ()
                  (setq-local orderless-matching-styles '(orderless-flex)))))
  ;; orderless を company のあいまい検索に使う設定
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook
              (lambda ()
                (setq-local orderless-matching-styles '(orderless-flex)))))
  )

  ;; corfu
(use-package corfu
  ;; emacs 30 の CLI では corfu がウィンドウを出せないので company を使う
  ;; emacs 31 が使えるようになったら corfu に一本化したい
  :if window-system
  :ensure t
  :after package
  :init
  (global-corfu-mode)
  :config
  (setopt corfu-cycle t
   	  corfu-auto t
   	  corfu-auto-delay 0.0
   	  corfu-auto-prefix 1
   	  corfu-on-exact-match 'show
   	  )
  )

;; corfu の補完にアイコンをつける
(use-package kind-icon
  :ensure t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; Company
(use-package company
  ;; GUI のときは corfu を使う
  :if (not window-system)
  :ensure t
  :after package
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  :bind (
	 :map company-active-map
	      ("<tab>" . company-complete-selection) ;; TABで候補を設定
	      ("C-S-h" . company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next)
	      )
  )

;; company のアイコン対応
(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  )