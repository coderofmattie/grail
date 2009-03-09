;;----------------------------------------------------------------------
;; frame.el
;; written by Mike Mattie
;;----------------------------------------------------------------------

;; parameters that need to be setup before frames are created.

(setq default-frame-alist
  (append '((cursor-color . "yellow")
            (mouse-color  . "red")
            (cursor-type  . "hollow")
            (width        . 85)
            (height       . 28))
    default-frame-alist))
