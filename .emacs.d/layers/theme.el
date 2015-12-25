;;------------------------------------------------------------------------------
;; layers/theme
;;------------------------------------------------------------------------------

(setq custom-safe-themes t)

(defun l-theme/themes-installed-p ()
  (loop for theme in l-theme/themes
	when (not (package-installed-p theme)) do (return nil)
	finally (return t)))

(if (boundp 'l-theme/themes)
    (unless (l-theme/themes-installed-p)
      (package-refresh-contents)
      (dolist (theme l-theme/themes)
	(when (not (package-installed-p theme))
	  (package-install theme)))))

(when (boundp 'l-theme/default) (load-theme l-theme/default))
