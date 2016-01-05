(defun µ/theme/installed-p ()
  "Check if all themes from settings is installed."
  (loop for theme in µ/theme/themes
        when (not (package-installed-p theme)) do (return nil)
        finally (return t)))

(setq-default custom-safe-themes t)

(unless (µ/theme/installed-p)
  (message "Not all themes are installed")
  
  (package-refresh-contents)
  
  (dolist (theme µ/theme/themes)
    (when (not (package-installed-p theme))
      (package-install theme))))

(let ((theme µ/theme/default))
  (if theme (load-theme theme)))
