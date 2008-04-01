;;----------------------------------------------------------------------
;; cedet.el
;;----------------------------------------------------------------------

(load-elisp-if-exists (concat grail-dist-elisp "cedet/common/cedet.el"))

(semantic-load-enable-minimum-features)

(semantic-load-enable-code-helpers)
