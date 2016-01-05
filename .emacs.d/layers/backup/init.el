(if µ/backup/enabled
    (progn
      (let ((directory µ/backup/directory))
        (unless (file-exists-p directory)
          (make-directory directory))
        
        (setq-default backup-directory-alist `(("." . ,directory))))

      (setq-default make-backup-files         t
                    backup-by-copying         t
                    version-control           t
                    delete-old-versions       t
                    delete-by-moving-to-trash t
                    keep-old-versions         3
                    keep-new-versions         3
                    auto-save-default         t
                    auto-save-timeout         20
                    auto-save-interval        200))

  (setq make-backup-files nil))
