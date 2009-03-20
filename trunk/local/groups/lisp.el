;;----------------------------------------------------------------------
;; lisp.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie 2009
;;----------------------------------------------------------------------

;; mic-paren fancy paren/delimited highlighting. It is particularly
;;           valuable for reverse highlighting regions.

;; the built-in paren mode is a fallback until mic-paren can be
;; activated.

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

  (grail-print-fn-to-scratch "activate-paren-mode-fallback" "activate the builtin paren mode a repair fallback")

  (defun activate-paren-mode-fallback ()
    (require 'paren)

    (add-hook 'emacs-lisp-mode-hook
      (lambda ()
        (show-paren-mode))) ))

;;----------------------------------------------------------------------
;; quack for scheme
;;----------------------------------------------------------------------

(defvar grail-groups-quack-installer
  (grail-define-installer "quack" "file" "http://www.neilvandyke.org/quack/quack.el")
  "the quack installer")

(grail-activate-with-recovery "lisp" quack grail-groups-quack-installer
  (setq-default
    quack-dir (grail-garuntee-dir-path (concat grail-state-path "quack/"))
    quack-default-program "mzscheme")

  (setq
    auto-mode-alist (append '(("\\.scheme$"    . quack-mode)
                              ("\\.scm$"       . quack-mode)
                               ) auto-mode-alist )) )

;; basic settings
(setq
  lisp-indent-offset 2)

;; if there are some long-term problems with mic-paren a backup solution is critical

(defun swap-paren-keys ()
  "bind the parentheses to the brace keys, while the shifted
   paren keys become the braces."
  (interactive)

  ;; make the parentheses a bit easier to type, less shifting.
  (local-set-key (kbd "[") (lambda () (interactive) (insert-char ?\( 1 nil)))
  (local-set-key (kbd "]") (lambda () (interactive) (insert-char ?\) 1 nil)))

  (local-set-key (kbd "(") (lambda () (interactive) (insert-char ?\[ 1 nil)))
  (local-set-key (kbd ")") (lambda () (interactive) (insert-char ?\] 1 nil))) )

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (swap-paren-keys)

    ;; this binding is very important. normal evaluation of defuns such as defvar
    ;; and defcustom do not change the default value because the form only sets
    ;; the value if nil.

    ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
    ;; this function/keybinding should be used exclusively to avoid frustrating
    ;; errors.
    (local-set-key (kbd "C-x e") 'eval-defun)

    ;; replace dired at point, far less useful to me than instrumenting a function.
    (local-set-key (kbd "C-x d") 'edebug-defun)

    ;; elisp doesn't need tags, find-function works just fine.
    (local-set-key (kbd "M-.")   'find-function) ))

;;----------------------------------------------------------------------
;; add lisp setup to other dialects of lisp and modes where lisp is
;; heavily used.
;;----------------------------------------------------------------------

;; IRC which I use almost exclusively for #emacs
(eval-after-load 'erc
  '(progn
     (add-hook 'erc-mode-hook 'swap-paren-keys)))

;; for the quack mz-scheme mode.
(eval-after-load 'quack
  '(add-hook 'quack-mode-hook 'swap-paren-keys))

