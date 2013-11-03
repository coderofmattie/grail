;;----------------------------------------------------------------------
;; lisp.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie 2009
;;----------------------------------------------------------------------

;; third party extensions for general lisp support.

(grail-load 'quack     (grail-define-installer "quack"
                         "file"
                         "http://www.neilvandyke.org/quack/quack.el"))

(grail-load 'clojure-mode  (grail-define-installer "clojure"
                            "pkg"
                            'clojure-mode))

;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------

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
  auto-mode-alist (append '(("\\.scheme$"    . scheme-mode)) auto-mode-alist ))

;;----------------------------------------------------------------------
;; clojure mode
;;----------------------------------------------------------------------

;; has not been used in ages. needs to be fast-forwarded for the new
;; environment it is in.

;; electric enter is forced in the keymap without any sort of option ...
;; turn that crap off by fixing the keymap.

;; (add-hook 'clojure-mode-hook
;;   (lambda ()
;;     (swap-paren-keys)
;;     (mattie-tab-switching)

;;     (substitute-key-definition 'reindent-then-newline-and-indent nil clojure-mode-map)
;;     (configure-for-evaluation 'slime-eval-defun 'slime-eval-last-expression 'slime-eval-region 'slime-eval-buffer) ))
