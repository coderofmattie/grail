;;----------------------------------------------------------------------
;; spell.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

(defun install-flyspell ()
  "install or update flyspell"
  (interactive)
  (deploy-url-elisp "http://www-sop.inria.fr/mimosa/Manuel.Serrano/flyspell/flyspell-1.7n.el" "flyspell"))

(require 'flyspell)

(setq-default
  ispell-program-name "aspell"                     ;; use aspell.
  flyspell-issue-message-flag nil)                 ;; don't bog down in bad english.

(add-hook 'text-mode-hook 'flyspell-mode)          ;; turn on regular flyspell mode text
                                                   ;; mode buffers.

(add-hook 'cperl-mode-hook 'flyspell-prog-mode)         ;; works quite nicely with cperl.
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)    ;; works nicely as well with elisp


