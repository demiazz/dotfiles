;;------------------------------------------------------------------------------
;; layers/ui
;;------------------------------------------------------------------------------

;; scroll bar, tool bar, menu bar

(if (and (boundp 'l-ui/scroll-bar) (not l-ui/scroll-bar))
    (scroll-bar-mode -1))

(if (and (boundp 'l-ui/tool-bar) (not l-ui/tool-bar))
    (tool-bar-mode -1))

(if (and (boundp 'l-ui/menu-bar) (not l-ui/menu-bar))
    (menu-bar-mode -1))

;; frame

(when (and window-system (boundp 'l-ui/frame-mode))
  (if (eq l-ui/frame-mode 'maximized)  (toggle-frame-maximized))
  (if (eq l-ui/frame-mode 'fullscreen) (toggle-frame-fullscreen)))

;; splash screen

(if (boundp 'l-ui/splash-screen)
    (setq inhibit-splash-screen (not l-ui/splash-screen)))

(if (boundp 'l-ui/scratch-message)
    (setq initial-scratch-message l-ui/scratch-message))

(if (boundp 'l-ui/initial-major-mode)
    (setq initial-major-mode l-ui/initial-major-mode))

;; marking text

(if (boundp 'l-ui/delete-selection)
    (delete-selection-mode l-ui/delete-selection))

(if (boundp 'l-ui/transient-mark)
    (transient-mark-mode l-ui/transient-mark))

(if (boundp 'l-ui/clipboard)
    (setq x-select-enable-clipboard l-ui/clipboard))

;; display settings

(when (boundp 'l-ui/indicate-empty-lines)
  (setq-default indicate-empty-lines l-ui/indicate-empty-lines)

  (if (not indicate-empty-lines) (toggle-indicate-empty-lines)))

;; line and column number in status line

(if (boundp 'l-ui/line-number) (line-number-mode l-ui/line-number))

(if (boundp 'l-ui/column-number) (column-number-mode l-ui/column-number))

;; indentation

(if (boundp 'l-ui/tab-width)
    (setq tab-width l-ui/tab-width))

(if (boundp 'l-ui/tabs)
    (setq-default indent-tabs-mode l-ui/tabs))

;; misc

(if (boundp 'l-ui/echo-keystrokes)
    (setq echo-keystrokes l-ui/echo-keystrokes))

(if (boundp 'l-ui/use-dialog-box)
    (setq use-dialog-box l-ui/use-dialog-box))

(if (boundp 'l-ui/visible-bell)
    (setq visible-bell l-ui/visible-bell))

(if (and (boundp 'l-ui/paren-mode) l-ui/paren-mode)
    (show-paren-mode))
