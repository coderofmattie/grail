;;
;; use socks5 for erc.
;;

(setq socks-noproxy '("localhost"))

(require 'socks)

(setq socks-server (list "priv" "localhost" 9999 5))

;; (setq erc-server-connect-function 'socks-open-network-stream)
