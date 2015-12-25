;;------------------------------------------------------------------------------
;; layers/backup
;;------------------------------------------------------------------------------

(use-package column-marker
  :ensure t
  :init 
  (add-hook 'prog-mode-hook
            (lambda ()
              (interactive)
              
              (set-face-attribute 'column-marker-1 nil :background "red")
              (set-face-attribute 'column-marker-2 nil :background "red")
              (set-face-attribute 'column-marker-3 nil :background "red")
              
              (when (boundp 'l-column-marker/rules)
                (let ((rule-1 (nth 0 l-column-marker/rules))
                      (rule-2 (nth 1 l-column-marker/rules))
                      (rule-3 (nth 2 l-column-marker/rules)))
                  (progn
                    (if rule-1 (column-marker-1 rule-1))
                    (if rule-2 (column-marker-2 rule-2))
                    (if rule-3 (column-marker-3 rule-3))))))))
