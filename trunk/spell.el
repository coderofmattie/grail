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

(defun flyspell-at-point-p ()
  "determine if the point is in a flyspell overlay. given a overlay list
   which may be nil, translate via predicate into boolean values which
   are then evaluated by or."
  (interactive)
  (let
    ((overlay-list (overlays-at (point))))

    (if overlay-list
      (eval (cons 'or
              (mapcar
                (lambda ( overlay )
                  (if (overlay-get overlay 'flyspell-overlay) t)) overlay-list)
              ))
    )))

;; create a tab context where tab will invoke flyspell-auto-correct-word
;; at the point.
(setq tab-context (cons
                    (lambda ()
                      (if (flyspell-at-point-p)
                        (progn
                          (flyspell-auto-correct-word)
                          t)))
                    tab-context))



