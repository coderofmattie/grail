;;----------------------------------------------------------------------
;; dwim-tab.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; License: LGPL-v3
;;----------------------------------------------------------------------

;; dwim-tab
;;
;; A highly overloaded "Tab" key hook.

(require 'dwim-tab-fn)

(defvar dwim-tab-context
  nil
  "a list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defun dwim-tab-add-context ( context-fn )
  "dwim-tab-add-context function

   Add a context function to the Tab hook. If the function detects that
   the point is within it's context, or turf then it should DTRT and
   return non-nil.
  "
  (push context-fn dwim-tab-context))

(defvar dwim-tab-fallback
  (lambda () (dabbrev-expand nil))
  "a fallback completion function")

(defun try-context-dwim ()
  "try-context-dwim

   Evaluate the tab context via the tab-context list of functions"
  (if dwim-tab-context
    (or-fn-list dwim-tab-context)
    nil))

(defun dwim-tab-generator ( complete-fn )
  "dwim-tab-generator

   creates a function for the tab key hook that will:

   1. try various context functions at the point.
   2. attempt completion when after non-whitespace.
   3. indent.

   The context functions are shared globally, while the
   completion function is bound to the function generated.
  "

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

(defun dwim-tab-localize ( &optional completion-context )
  ;; when setting up major/minor modes adapt my global key-bindings to
  ;; the modes of the buffer. Local key-bindings that shadow my global
  ;; key-bindings are unset or replaced with contextualized variations
  ;; of my global defaults.

  (local-set-key (kbd "<tab>") (dwim-tab-generator (or completion-context dwim-tab-fallback))) )

(provide 'dwim-tab)
