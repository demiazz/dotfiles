;;------------------------------------------------------------------------------
;; Selection
;;------------------------------------------------------------------------------

;; Delete selection when start typing

(delete-selection-mode µ/edit/delete-selection)

;; Transient mark like a classic editors

(transient-mark-mode µ/edit/transient-mark)

;;------------------------------------------------------------------------------
;; Clipboard
;;------------------------------------------------------------------------------

(setq-default x-select-enable-clipboard t)

;;------------------------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------------------------

(setq-default tab-width        µ/edit/tab-width
              indent-tabs-mode µ/edit/tabs)
