;;----------------------------------------------------------------------
;; frame.el
;; written by Mike Mattie
;;----------------------------------------------------------------------

;; parameters that need to be setup before frames are created.

(setq default-frame-alist
  (append '((cursor-color . "yellow")
            (cursor-type  . "hollow"))
    default-frame-alist))
