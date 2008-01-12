;;----------------------------------------------------------------------
;; spell.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------
(require 'flyspell)

(setq-default
  ispell-program-name "aspell"                     ;; use aspell.
  flyspell-issue-message-flag nil)                 ;; don't bog down in bad english.

(add-hook 'text-mode-hook 'flyspell-mode)          ;; turn on regular flyspell mode text
                                                   ;; mode buffers.

(add-hook 'cperl-mode-hook 'flyspell-prog-mode)         ;; works quite nicely with cperl.
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)    ;; works nicely as well with elisp

(defun correct-over-flyspell ()
  "auto-correct the word if over a flyspell region, return t only
   if over a fly-spell region"
  (interactive)

  (if (mode-overlay-at-point-p 'flyspell-overlay)
    (progn
      (flyspell-auto-correct-word)
      t)))

;; create a tab context where tab will invoke flyspell-auto-correct-word
;; at the point.
(setq tab-context (cons 'correct-over-flyspell tab-context))



