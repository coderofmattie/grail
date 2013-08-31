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

(grail-load 'mic-paren (grail-define-installer "mic-paren"
                         "file"
                         "http://www.emacswiki.org/cgi-bin/emacs/download/mic-paren.el"))

(grail-load 'slime     (grail-define-installer "slime"
                          "pkg"
                          'slime))

(grail-load 'slime-repl  (grail-define-installer "slime-repl"
                          "pkg"
                          'slime-repl))

(grail-load 'quack     (grail-define-installer "quack"
                         "file"
                         "http://www.neilvandyke.org/quack/quack.el"))

(grail-load 'clojure-mode  (grail-define-installer "clojure"
                            "pkg"
                            'clojure-mode))

(grail-load 'elein (grail-define-installer "elein"
                     "pkg"
                     'elein))

(setq
  paren-showing t
  show-paren-style 'parenthesis
  show-paren-delay 1
  paren-sexp-mode 'match)

(grail-set-faces
  (paren-face-match (background "grey20")))

(paren-activate)

;;----------------------------------------------------------------------
;; SLIME
;;----------------------------------------------------------------------
(setq
  slime-net-coding-system 'utf-8-unix)

(slime-setup '(slime-repl))

(setq
  slime-words-of-encouragement '("The name is Bond. James Bond."
                                 "These are your father's parentheses. Elegant weapons from a more civilized age."
                                 "We were on the edge of the desert when the Emacs took hold."
                                 "Mine says: Desert Eagle ... .50"))

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
;; clojure/SLIME
;;---------------------------------------------------------------------

;; font-lock and key setup on the REPL
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(add-hook 'slime-repl-mode-hook 'swap-paren-keys)

;; --> elein

;; elein tries to pass an encoding option to the lein swank command. This blows
;; up horribly causing a runtime exception in the swank-clojure code. disabling
;; it below makes everyone happy ...
(setq elein-swank-options "")

;;----------------------------------------------------------------------
;; clojure mode
;;----------------------------------------------------------------------

;; electric enter is forced in the keymap without any sort of option ...
;; turn that crap off by fixing the keymap.

(add-hook 'clojure-mode-hook
  (lambda ()
    (swap-paren-keys)
    (mattie-tab-switching)

    (substitute-key-definition 'reindent-then-newline-and-indent nil clojure-mode-map)
    (configure-for-evaluation 'slime-eval-defun 'slime-eval-last-expression 'slime-eval-region 'slime-eval-buffer) ))
