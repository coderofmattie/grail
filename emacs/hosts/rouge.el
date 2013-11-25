;;----------------------------------------------------------------------
;; rouge.el
;;
;; rouge system specific configuration
;;----------------------------------------------------------------------
(require 'cm-string)

(require 'file-utilities)
(require 'perforce-utilities)
(require 'tags-uber)

(pforce-add-tree (files-make-path-rooted-to-home "work/import-codebase"))
(pforce-add-tree (files-make-path-rooted-to-home "work/core-codebase"))
(pforce-add-tree (files-make-path-rooted-to-home "work/import-codebase"))

(perforce-activate-with-working-copy)

(tags-uber-update-for-mode "cperl-mode"
  '("inventory" "/home/codermattie/work/inventory-codebase"))

;; (require 'erc)
;; (require 'socks)

;; (add-hook 'erc-mode-hook
;;   (lambda ()
;;     (setq socks-noproxy '("localhost"))
;;     (setq erc-server-connect-function 'socks-open-network-stream)

;;     (setq socks-server (list "tor proxy" "localhost" 9050 5)) )
;;   t)
