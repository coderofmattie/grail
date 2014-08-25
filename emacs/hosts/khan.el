;;
;; use socks5 for erc.
;;

(setq socks-noproxy '("localhost"))

(require 'socks)

(setq socks-server (list "priv" "localhost" 6666 5))

(setq erc-server-connect-function 'socks-open-network-stream)
