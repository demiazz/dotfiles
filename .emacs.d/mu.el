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

;;------------------------------------------------------------------------------
;; use-package
;;------------------------------------------------------------------------------

(defconst µ/use-package-path
  "Path to `use-package`."
  (expand-file-name "use-package" user-emacs-directory))

;; Add use-package path to load-path list

(add-to-list 'load-path µ/use-package-path)

;; Try load use-package, and clone them from Git, if it's not exists

(unless (require 'use-package nil 'noerror)
  (let* ((log (get-buffer-create "*use-package-installation*"))
         (git (or (executable-find "git")
                  (error "Unable to find `git` executable")))
         (url "https://github.com/jwiegley/use-package")

         (process-connection-type nil)

         (status
          (call-process
           git nil `(,log t) t "--no-pager" "clone" "-v" url µ/use-package-path)))

    (if (zerop status)
        (message "Package `use-package` successfully installed")
      (error "Couldn't clone `use-package` from the Git repository `%s`" url))))

;;------------------------------------------------------------------------------
;; Layers
;;------------------------------------------------------------------------------

(defconst µ/layers-path
  (expand-file-name "layers" user-emacs-directory)
  "Layer's root path.")

(defun µ/layer-path-p (file-path)
  "Is FILE-PATH is a layer's path?"
  (let* ((file-name     (file-name-nondirectory file-path))
         (is-directory  (file-directory-p file-path))
         (is-right-name (string-match "^[^\.|\+].*$" file-name)))
    (and is-directory is-right-name)))

(defun µ/group-path-p (file-path)
  "Is FILE-PATH is a group's path?"
  (let* ((file-name     (file-name-nondirectory file-path))
         (is-directory  (file-directory-p file-path))
         (is-right-name (string-match "\+.*$" file-name)))
    (and is-directory is-right-name)))

(defun µ/discover-layers (&optional root)
  "Recursively find layers in layers path or given ROOT."
  (let* ((layers-root (if root root µ/layers-path))
         (files       (directory-files layers-root t nil t))
         (result      '()))
    (dolist (path files result)
      (cond ((µ/layer-path-p path)
             (let* ((layer-name (file-name-base path))
                    (layer      (cons layer-name path)))
               (setq result (cons layer result))))
            ((µ/group-path-p path)
             (let ((layers (µ/discover-layers path)))
               (setq result (append result layers))))))
    result))

(defun µ/load-layer (layer)
  "Load LAYER's files."
  (let ((layer-path  (cdr layer))
        (files       '("config" "init" "keybindings" "funcs")))
    (dolist (file-name files)
      (let ((file-path (expand-file-name file-name layer-path)))
        (if (file-exists-p (concat file-path ".el"))
            (load file-path))))))

(defun µ/load-layers (layers)
  "Load given LAYERS."
  (let ((available-layers (µ/discover-layers)))
    (dolist (layer-name layers)
      (let ((layer (assoc (symbol-name layer-name) available-layers)))
        (if layer (µ/load-layer layer))))))

;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defun µ/list-to-assoc (source)
  (if (oddp (length source))
      (error "List must have even count of elements for converting to alist."))

  (let ((acc    '())
        (result '()))
    (dolist (e source)
      (setq acc (cons e acc))

      (when (= (length acc) 2)
        (setq result (cons (reverse acc) result))
        (setq acc    '())))

    (reverse result)))

(defun µ/default-settings (layer settings)
  "Set SETTINGS if is not exists."
  (dolist (setting settings)
    (let* ((setting-name (symbol-name (car setting)))
	   (layer-name   (symbol-name layer))
	   (µ-name       (intern
			  (concat "µ/" layer-name "/" setting-name)))
	   (value        (cdr setting)))

      (if (not (boundp µ-name))
	  (set µ-name value)))))

(defmacro µ/settings (layer &rest settings)
  "Set SETTINGS."
  `(dolist (setting (µ/list-to-assoc ',settings))
    (let* ((setting-name (symbol-name (car setting)))
           (layer-name   (symbol-name ',layer))
           (µ-name       (intern
                          (concat "µ/" layer-name "/" setting-name)))
           (value        (car (cdr setting))))

      (set µ-name value))))

;;------------------------------------------------------------------------------
;; Precompile
;;------------------------------------------------------------------------------

(defun µ/precompile ()
  "Precompile core and layers"
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))
