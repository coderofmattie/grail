;;----------------------------------------------------------------------
;; frame.el
;; written by Mike Mattie
;;----------------------------------------------------------------------

;; parameters that need to be setup before frames are created.

(setq default-frame-alist
  (append '((cursor-color . "yellow")
            (mouse-color  . "red")
            (cursor-type  . "hollow"))
    default-frame-alist))
