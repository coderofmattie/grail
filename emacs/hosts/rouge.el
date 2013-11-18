;;----------------------------------------------------------------------
;; rouge.el
;;
;; rouge system specific configuration
;;----------------------------------------------------------------------

(require 'cm-string)
(require 'perforce-utilities)

(defun relative-to-home-path ( path &optional dir )
  (let
    ((home-dir (getenv "HOME")))

    (concat "/"
      (string-join "/" (split-string (concat home-dir "/" path) "[/]" t))
      (if dir "/" "")) ))

(pforce-add-tree (relative-to-home-path "/media/codebase/perforce/classic" t))
(pforce-add-tree (relative-to-home-path "work/import-codebase" t))
(pforce-add-tree (relative-to-home-path "work/core-codebase" t))
(pforce-add-tree (relative-to-home-path "work/import-codebase" t))

(perforce-activate-with-working-copy)

;; (require 'erc)
;; (require 'socks)

;; (add-hook 'erc-mode-hook
;;   (lambda ()
;;     (setq socks-noproxy '("localhost"))
;;     (setq erc-server-connect-function 'socks-open-network-stream)

;;     (setq socks-server (list "tor proxy" "localhost" 9050 5)) )
;;   t)
