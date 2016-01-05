;;------------------------------------------------------------------------------
;; Bars
;;------------------------------------------------------------------------------

(unless µ/ui/scroll-bar (scroll-bar-mode -1))
(unless µ/ui/tool-bar   (tool-bar-mode -1))
(unless µ/ui/menu-bar   (menu-bar-mode -1))

;;------------------------------------------------------------------------------
;; Frame
;;------------------------------------------------------------------------------

(when window-system
  (let ((mode µ/ui/frame-mode))
    (cond ((equal mode 'maximized)  (toggle-frame-maximized))
          ((equal mode 'fullscreen) (toggle-frame-fullscreen)))))

;;------------------------------------------------------------------------------
;; Font
;;------------------------------------------------------------------------------

(when window-system
  (let ((family µ/ui/font-family)
        (size   µ/ui/font-size))
    (if (member family (font-family-list))
        (set-face-attribute 'default nil :font family))
    (set-face-attribute 'default nil :height (* size 10))))

;;------------------------------------------------------------------------------
;; Splash screen
;;------------------------------------------------------------------------------

(setq-default inhibit-splash-screen (not µ/ui/splash-screen))

;;------------------------------------------------------------------------------
;; Scratch
;;------------------------------------------------------------------------------

(setq-default initial-scratch-message µ/ui/scratch-message
              initial-major-mode      µ/ui/major-mode)

;;------------------------------------------------------------------------------
;; Column markers
;;------------------------------------------------------------------------------

(use-package column-marker
  :ensure t
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (set-face-attribute 'column-marker-1 nil :background "red")
              (set-face-attribute 'column-marker-2 nil :background "red")
              (set-face-attribute 'column-marker-3 nil :background "red")
              
              (let ((rule-1 (nth 0 µ/ui/column-rules))
                    (rule-2 (nth 1 µ/ui/column-rules))
                    (rule-3 (nth 2 µ/ui/column-rules)))
                (if rule-1 (column-marker-1 rule-1))
                (if rule-2 (column-marker-2 rule-2))
                (if rule-3 (column-marker-3 rule-3))))))

;;------------------------------------------------------------------------------
;; Line and column numbers in status line
;;------------------------------------------------------------------------------

(line-number-mode   µ/ui/line-number)
(column-number-mode µ/ui/column-number)

;;------------------------------------------------------------------------------
;; Line numbers and empty lines
;;------------------------------------------------------------------------------

(toggle-indicate-empty-lines)

(use-package nlinum
  :ensure t
  :init
  (setq-default nlinum-format µ/ui/linum-format)
  (global-linum-mode))

;;------------------------------------------------------------------------------
;; Paren mode
;;------------------------------------------------------------------------------

(show-paren-mode)

;;------------------------------------------------------------------------------
;; Miscellaous
;;------------------------------------------------------------------------------

(setq-default echo-keystrokes 0.1
              use-dialog-box  nil
              visible-bell    nil)

