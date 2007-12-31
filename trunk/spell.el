;;----------------------------------------------------------------------
;; spell.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

(defun install-flyspell ()
  "install or update flyspell"
  (interactive)
  (dl-elisp "http://www-sop.inria.fr/mimosa/Manuel.Serrano/flyspell/flyspell-1.7n.el" "flyspell"))

(require 'flyspell)

(setq-default
  aspell-program-name "aspell")               ;; use aspell.


