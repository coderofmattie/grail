;;----------------------------------------------------------------------
;; dwim-tab.el
;; Primary Author: Mike Mattie
;; Copyright:
;; License: LGPL-v3
;;----------------------------------------------------------------------

(require 'mattie-elisp)

(defun try-list (list)
  "iterate through the list of functions. If a function returns t for
   success terminate the iteration. It's a fancy or that assumes a list
   of functions."
  (catch 'terminate
    (dolist (func list)
      (if (funcall func)
        (throw 'terminate t)))
    nil
    ))

(defvar tab-context
  nil
  "a list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defun eval-tab-context ()
  "evaluate the tab context via the tab-context list of functions"
  (if tab-context
    (try-list tab-context)
    nil))

(defun dwim-tab-generator (completion-context)
  ;; generate a contextualized flavor of the tab key behavior.

  (lexical-let
    ((completion-function
       ;; TODO, handle a lambda coming in.
       ;; just a string selecting what is hardwired in here.

       (cond
         ((functionp completion-context) completion-context)
         ((symbolp completion-context) completion-context)
         ;; use lisp-complete-symbol for elisp
         ((string-equal completion-context "elisp") 'lisp-complete-symbol)

         ;; fall back on dabbrev
         ((lambda () (dabbrev-expand nil)))
         )))

    ;; If I create a un-interned symbol I think my problems will go away.
    (bind-eval-lambda "dwim-tab"
      (lambda ()
        "Complete if point is at end of a word, otherwise indent line."
        (interactive)

        ;; first try the tab context which should override the
        ;; general tab behavior only when the text or properties
        ;; essentially guarantee to DTRT
        (unless (eval-tab-context)
          (if (looking-at "\\>")
            ;; fall back on completion or indentation
            (funcall completion-function)
            (indent-for-tab-command)))
        ))
    ))

(defun dwim-tab-localize ( completion-context )
  ;; when setting up major/minor modes adapt my global key-bindings to
  ;; the modes of the buffer. Local key-bindings that shadow my global
  ;; key-bindings are unset or replaced with contextualized variations
  ;; of my global defaults.

  (local-unset-key (kbd "<M-tab>"))
  (local-unset-key (kbd "<S-tab>"))

  (local-set-key (kbd "<tab>") (dwim-tab-generator completion-context)) )

(provide 'dwim-tab)
