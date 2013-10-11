;;----------------------------------------------------------------------
;; tramp profile
;;
;; tramp setup for emacs.
;;----------------------------------------------------------------------

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(require 'tramp)

; tramp-chunksize is defensive to reduce problems with hangs from sending
; to large of chunks

(setq
  tramp-default-method "ssh"
  tramp-chunksize 500 )



