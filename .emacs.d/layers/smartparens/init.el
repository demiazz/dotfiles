(use-package smartparens
  :ensure t
  :pin    "melpa-stable"
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode #'smartparens-mode))
