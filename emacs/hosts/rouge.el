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
  '("inventory" "/home/codermattie/work/inventory-codebase")
  '("system"
     "/usr/local/lib/perl/5.14.2"
     "/usr/local/share/perl/5.14.2"
     "/usr/lib/perl5"
     "/usr/share/perl5"
     "/usr/lib/perl/5.14"
     "/usr/share/perl/5.14"
     "/usr/local/lib/site_perl")
  '("cobalt-web"
     "/web/lib/"
     "/web/cobalt/"
     "/web/auto/lib/"
     "/web/auto/import/lib/"
     "/web/auto/export/inventory/"))

;; (require 'erc)
;; (require 'socks)

;; (add-hook 'erc-mode-hook
;;   (lambda ()
;;     (setq socks-noproxy '("localhost"))
;;     (setq erc-server-connect-function 'socks-open-network-stream)

;;     (setq socks-server (list "tor proxy" "localhost" 9050 5)) )
;;   t)
