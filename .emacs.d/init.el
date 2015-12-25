;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("marmalade"    . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa"        . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; use-package

(defconst use-package-directory
  (expand-file-name "use-package" user-emacs-directory))

(add-to-list 'load-path use-package-directory)

(unless (require 'use-package nil 'noerror)
  (let* ((log (get-buffer-create "*use-package-installation*"))
         (git (or (executable-find "git")
                  (error "Unable to find `git` executable")))
         (url "https://github.com/jwiegley/use-package")

         (process-connection-type nil)

         (status
          (call-process
           git nil `(,log t) t "--no-pager" "clone" "-v" url use-package-directory)))

    (if (zerop status)
        (message "Package `use-package` successfully installed")
      (error "Couldn't clone `use-package` from the Git repository `%s`" url))))

;;------------------------------------------------------------------------------
;; Configuration file
;;------------------------------------------------------------------------------

(load-file (expand-file-name "config.el" user-emacs-directory))

;;------------------------------------------------------------------------------
;; Layers
;;------------------------------------------------------------------------------

(if (boundp 'demiazz/layers)
    (dolist (layer demiazz/layers)
      (load-file
       (expand-file-name (format "layers/%s.el" layer)
                         user-emacs-directory))))
