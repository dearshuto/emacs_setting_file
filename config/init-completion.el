;; モードライン
(use-package vertico
  :ensure t
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
)

;; ミニバッファに詳細なドキュメントやファイル情報を付与
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

  ;; corfu
(use-package corfu
  ;; emacs 30 以前の CLI では corfu がウィンドウを出せないので注意
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-on-exact-match 'show)
  )

;; corfu の補完にアイコンをつける
(use-package kind-icon
  :ensure t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
