;;----------------------------------------------------------------------
;; dwim-tab.el
;; Primary Author: Mike Mattie
;; Copyright:
;; License: LGPL-v3
;;----------------------------------------------------------------------
(require 'dwim-tab-fn)

(defvar dwim-tab-context
  nil
  "a list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defvar dwim-tab-fallback
  (lambda () (dabbrev-expand nil))
  "fallback completion function")

(defun try-context-dwim ()
  "evaluate the tab context via the tab-context list of functions"
  (if dwim-tab-context
    (or-fn-list dwim-tab-context)
    nil))

(defun dwim-tab-generator ( complete-fn )
  ;; generate a contextualized flavor of the tab key behavior.

  (lexical-let
    ((completion-function (or complete-fn dwim-tab-fallback)))

    ;; If I create a un-interned symbol I think my problems will go away.
    (bind-eval-lambda "dwim-tab"
      (lambda ()
        "Complete if point is at end of a word, otherwise indent line."
        (interactive)

        ;; first try the tab context which should override the
        ;; general tab behavior only when the text or properties
        ;; essentially guarantee to DTRT
        (unless (try-context-dwim)
          (if (looking-at "\\>")
            ;; fall back on completion or indentation
            (funcall completion-function)
            (indent-for-tab-command))) )) ))

(defun dwim-tab-localize ( completion-context )
  ;; when setting up major/minor modes adapt my global key-bindings to
  ;; the modes of the buffer. Local key-bindings that shadow my global
  ;; key-bindings are unset or replaced with contextualized variations
  ;; of my global defaults.

  (local-set-key (kbd "<tab>") (dwim-tab-generator completion-context)) )

(provide 'dwim-tab)
