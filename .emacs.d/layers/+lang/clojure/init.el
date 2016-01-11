(use-package cider
  :ensure t
  :pin    "melpa-stable"
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook   'cider-turn-on-eldoc-mode))

(use-package clojure-mode
  :ensure t
  :pin    "melpa-stable"
  :init
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '(".* boot"    . clojure-mode)))
