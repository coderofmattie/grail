;;----------------------------------------------------------------------
;; spell.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------
(require 'flyspell)
(require 'dwim-tab)

(setq-default
  ispell-program-name "aspell"                     ;; use aspell.
  flyspell-issue-message-flag nil)                 ;; don't bog down in bad English.

;; turn on regular flyspell mode text mode buffers.
;; disabled: something is really wrong here. "no topics found" pops up
;;           and key insertion fails when the underlining begins.

;; (add-hook 'text-mode-hook       'flyspell-mode)

;; (grail-tarball-installer
;;   "http://www.dr-qubit.org/download.php?file=predictive/auto-overlays.tar.gz"
;;   "auto-overlays.tar"
;;   "gz")

;; programming modes.

(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

(eval-after-load 'cperl-mode
  '(add-hook 'cperl-mode-hook   'flyspell-prog-mode))

(eval-after-load 'cc-mode
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode))

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





