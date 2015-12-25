;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

;; layers/font

(setq l-font/family "Monaco"
      l-font/size   14)

;; layers/ui

(setq l-ui/frame-mode           'maximized
      l-ui/splash-screen        nil
      l-ui/scratch-message      nil
      l-ui/initial-major-mode   'lisp-mode
      l-ui/scroll-bar           nil
      l-ui/tool-bar             nil
      l-ui/menu-bar             (and window-system (eq system-type 'darwin))
      l-ui/delete-selection     t
      l-ui/transient-mark       t
      l-ui/clipboard            t
      l-ui/indicate-empty-lines t
      l-ui/tab-width            2
      l-ui/tabs                 nil
      l-ui/echo-keystrokes      0.1
      l-ui/use-dialog-box       nil
      l-ui/visible-bell         t
      l-ui/paren-mode           t
      l-ui/line-number          t
      l-ui/column-number        t)

;; layers/theme

(setq l-theme/themes  '(color-theme-sanityinc-tomorrow
			twilight-theme
			twilight-anti-bright-theme))
(setq l-theme/default 'twilight)

;; layers/backup

(setq l-backup/enabled        t
      l-backup/directory      (expand-file-name "backups" user-emacs-directory))

;; layers/column-marker

(setq l-column-marker/rules '(80 100 120))

;;------------------------------------------------------------------------------
;; Layers
;;------------------------------------------------------------------------------

(setq demiazz/layers '(
                       font
                       ui
                       theme
                       backup
                       column-marker
                       ))
