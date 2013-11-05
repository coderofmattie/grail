;;----------------------------------------------------------------------
;; clojure support
;;----------------------------------------------------------------------

;; third party extensions for general lisp support.

(grail-load 'clojure-mode  (grail-define-installer "clojure"
                            "pkg"
                            'clojure-mode))

;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------

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
