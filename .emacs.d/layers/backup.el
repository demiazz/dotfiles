;;------------------------------------------------------------------------------
;; layers/backup
;;------------------------------------------------------------------------------

(if (boundp 'l-backup/enabled)
    (if l-backup/enabled
	(progn
	  (when (boundp 'l-backup/directory)
	    (if (not (file-exists-p l-backup/directory))
		(make-directory l-backup/directory t))
	    (setq backup-directory-alist `(("." . ,l-backup/directory))))

	  (setq make-backup-files         t
          backup-by-copying         t
          version-control           t
          delete-old-versions       t
          delete-by-moving-to-trash t
          keep-old-versions         3
          keep-new-versions         3
          auto-save-default         t
          auto-save-timeout         20
          auto-save-interval        200))
  (setq make-backup-files nil)))
