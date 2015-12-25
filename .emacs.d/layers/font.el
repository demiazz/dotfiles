;;------------------------------------------------------------------------------
;; layers/font
;;------------------------------------------------------------------------------

(when window-system
  (if (boundp 'l-font/family)
      (let ((family l-font/family))
        (if (member family (font-family-list))
            (set-face-attribute 'default nil :font family))))
  (if (boundp 'l-font/size)
      (set-face-attribute 'default nil :height (* l-font/size 10))))
