;;----------------------------------------------------------------------
;; tramp profile
;;
;; tramp setup for emacs.
;;----------------------------------------------------------------------

(require 'tramp)

; tramp-chunksize is defensive to reduce problems with hangs from sending
; to large of chunks

(setq
  tramp-default-method "ssh"
  tramp-chunksize 500 )
