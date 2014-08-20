(setq hyperspec-dir nil)

;; (setq hyperspec-dir
;;   (grail-fetch-docs "hyperspec"
;;     (grail-define-installer "hyperspec" "tar:gz"
;;       "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
;;     1))

;;
;; basic setup
;;

;; (setq common-lisp-hyperspec-default (concat common-lisp-hyperspec-root "Body/00_.htm"))
;; (code-documentation-setup "lisp-mode-docs" "lisp-mode" common-lisp-hyperspec-default)

common-lisp-hyperspec-root (concat "file://" hyperspec-dir "/")

