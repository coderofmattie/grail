;;----------------------------------------------------------------------
;; lisp.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie 2009
;;----------------------------------------------------------------------

;; third party extensions for general lisp support.

;;----------------------------------------------------------------------
;; parentheses matching
;;----------------------------------------------------------------------

;; mic-paren fancy paren/delimited highlighting. It is particularly
;;           valuable for reverse highlighting regions.

(grail-load 'mic-paren (grail-define-installer "mic-paren"
                         "file"
                         "http://www.emacswiki.org/cgi-bin/emacs/download/mic-paren.el"))

(grail-load 'quack     (grail-define-installer "quack"
                         "file"
                         "http://www.neilvandyke.org/quack/quack.el"))

(grail-load 'clojure-mode  (grail-define-installer "clojure"
                            "pkg"
                            'clojure-mode))

;;----------------------------------------------------------------------
;; paren mode configuration - most important mode of them all
;;----------------------------------------------------------------------
(setq
  paren-showing t
  show-paren-style 'parenthesis
  show-paren-delay 1
  paren-sexp-mode 'match)

(grail-set-faces
  (paren-face-match (background "grey20")))

(paren-activate)

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
