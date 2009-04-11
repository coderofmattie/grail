;;;----------------------------------------------------------------------
;; web.el
;; written by Mike Mattie
;;;----------------------------------------------------------------------

;; load the w3m browser function and use it for viewing url's .

;; NOTE: find-file can be configured to use different functions to
;;       open a file based on regex . function pairs. Later it
;;       would be nice to use that scheme.

(require 'w3m-load)
(setq
  browse-url-browser-function 'w3m-goto-url
  w3m-default-display-inline-images t)
