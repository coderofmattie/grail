;;----------------------------------------------------------------------
;; lisp.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie 2009
;;----------------------------------------------------------------------

;; third party extensions lisp support.

;;----------------------------------------------------------------------
;; parentheses matching
;;----------------------------------------------------------------------

;; mic-paren fancy paren/delimited highlighting. It is particularly
;;           valuable for reverse highlighting regions.

(defvar grail-groups-micparen-installer
  (grail-define-installer "mic-paren" "file" "http://www.emacswiki.org/cgi-bin/emacs/download/mic-paren.el")
  "the mic-paren installer")

(unless (grail-activate-with-recovery "lisp" mic-paren grail-groups-micparen-installer
          (setq
            paren-showing t
            show-paren-style 'parenthesis
            show-paren-delay 1
            paren-sexp-mode 'match )

          (paren-activate)

          (grail-set-faces
            (paren-face-match (background "grey20"))) )

  ;; do the repair thingy
  (grail-dup-error-to-scratch "the lisp style is hobbled by the broken mic-paren loading")

  ;; the built-in paren mode is a fallback until mic-paren can be
  ;; activated.
  (grail-print-fn-to-scratch "activate-paren-mode-fallback" "activate the builtin paren mode a repair fallback")

  (defun activate-paren-mode-fallback ()
    (require 'paren)

    (add-hook 'emacs-lisp-mode-hook
      (lambda ()
        (show-paren-mode))) ))

;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------

;; use quack mode for scheme which seems to be the most advanced of
;; the modes.

(defvar grail-groups-quack-installer
  (grail-define-installer "quack" "file" "http://www.neilvandyke.org/quack/quack.el")
  "the quack installer")

(grail-activate-with-recovery "lisp" quack grail-groups-quack-installer
  (setq-default
    quack-dir (grail-garuntee-dir-path (concat grail-state-path "quack/"))
    quack-default-program default-scheme-interpreter

    ;; don't always prompt for the scheme interpreter. Use the default.
    quack-run-scheme-always-prompts-p nil

    ;; don't use customize to save settings.
    quack-options-persist-p nil

    ;; fontify using Emacs faces, don't use a drscheme theme clone.
    quack-fontify-style 'emacs

    ;; tabs are evil
    quack-tabs-are-evil-p t)

  ;; quack adds all the typical extensions when loaded, so only add non-standard
  ;; ones.
  (setq
    auto-mode-alist (append '(("\\.scheme$"    . scheme-mode)) auto-mode-alist )) )

;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------

(defconst slime-project-page "http://common-lisp.net/project/slime/"
  "the SLIME project page")

(defvar grail-groups-slime-installer
  (grail-define-installer "slime" "cvs"
    ":pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot")
  "the slime installer")

(grail-activate-with-recovery "lisp" slime grail-groups-slime-installer
  (setq inferior-lisp-program "sbcl")
  (slime-setup))


