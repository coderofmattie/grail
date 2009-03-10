;;----------------------------------------------------------------------
;; spell.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------
(require 'flyspell)
(require 'dwim-tab)

(setq-default
  ispell-program-name "aspell"                     ;; use aspell.
  flyspell-issue-message-flag nil)                 ;; don't bog down in bad english.

(add-hook 'text-mode-hook       'flyspell-mode)          ;; turn on regular flyspell mode text
                                                         ;; mode buffers.
;; (grail-tarball-installer
;;   "http://www.dr-qubit.org/download.php?file=predictive/auto-overlays.tar.gz"
;;   "auto-overlays.tar"
;;   "gz")

;; programming modes.

;;(add-hook 'cperl-mode-hook      'flyspell-prog-mode)
;;(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;;(add-hook 'c-mode-common-hook   'flyspell-prog-mode)

(grail-set-faces
  (flyspell-incorrect (underline t)))

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

(dwim-tab-globalize-context 'correct-over-flyspell)





