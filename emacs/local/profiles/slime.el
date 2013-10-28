;;----------------------------------------------------------------------
;; slime
;;
;; slime profile for common lisp coding
;;----------------------------------------------------------------------

(grail-load 'slime     (grail-define-installer "slime"
                         "cvs"
                         ":pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot" ))

;; (grail-load 'slime-repl  (grail-define-installer "slime-repl"
;;                           "pkg"
;;                           'slime-repl))

;;----------------------------------------------------------------------
;; SLIME
;;----------------------------------------------------------------------
(require 'slime)

(setq
  slime-net-coding-system 'utf-8-unix

  inferior-lisp-program "sbcl"

  slime-words-of-encouragement '("The name is Bond. James Bond."
                                  "These are your father's parentheses. Elegant weapons from a more civilized age."
                                  "We were on the edge of the desert when the Emacs took hold."
                                  "Mine says: Desert Eagle ... .50"))

;; (add-hook 'lisp-mode-hook (lambda ()
;;                             (slime-mode t)))

(add-hook 'inferior-lisp-mode-hook
  (lambda () (inferior-slime-mode t)))

;; (slime-setup '(slime-repl))


