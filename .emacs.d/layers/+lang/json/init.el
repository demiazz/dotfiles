(use-package json-mode
  :ensure t
  :pin    "melpa-stable"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level µ/json/indent-level))))
