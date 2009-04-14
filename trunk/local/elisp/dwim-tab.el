;;; dwim-tab.el --- DWIM tab overloading.

;; Copyright (C) 2008 Mike Mattie

;; Author: Mike Mattie <codermattie@gmail.com>
;; Maintainer: Mike Mattie <codermattie@gmail.com>
;; Created: 2007-12-15
;; Version: 0.0.1
;; Keywords: tab,dwim
;; License: LGPL <http://www.gnu.org/licenses/lgpl.html>


;; A highly overloaded "Tab" key implementation targeted at programming modes.

(require 'dwim-tab-fn)

(eval-when-compile
  (require 'cl))

(defvar dwim-tab-global-context nil
  "A list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defvar dwim-tab-fallback 'hippie-expand
  "A fallback completion function which defaults to hippie expand.
   set dwim-tab-fallback to a function, or a list of
   functions. When more than one function is specified the
   functions will be executed in order until one or none returns
   non-nil.")

(defvar dwim-tab-local-context nil
  "A function, or list of functions ")

(make-variable-buffer-local 'dwim-tab-local-context)

(defun dwim-tab-local-context ( &rest context-functions )
  "dwim-tab-local-context function-list

   Add a context function to the Tab hook. If the function detects that
   the point is within it's context, or turf then it should DTRT and
   return non-nil.
  "
  (setq dwim-tab-local-context (append context-functions dwim-tab-local-context)))

(defun dwim-tab-globalize-context ( context-fn )
  "dwim-tab-globalize-context function

   Add a context function to the Tab hook. If the function detects that
   the point is within it's context, or turf then it should DTRT and
   return non-nil.
  "
  (add-to-list 'dwim-tab-global-context context-fn t))

(defun dwim-tab-new-fallback ( complete-fn )
  (setq dwim-tab-fallback complete-fn))

(defun dwim-tab-add-fallback ( complete-fn )
  (setq dwim-tab-fallback (cons complete-fn dwim-tab-fallback)))

(defun try-context-dwim ( context )
  "try-context-dwim CONTEXT

   Search through the CONTEXT function list for a applicable DWIM.
  "
  (if context
    (or-fn-list context)
    nil))

(defun try-complete-dwim ( complete )
  "try-complete-dwim COMPLETE

   Search through the COMPLETE function list for a expansion. This
   function catches 'terminate-complete.
  "
  (if complete
    (let
      ((point-before (point)))

      (catch 'terminate-complete
        (dolist (fn complete)
          (funcall fn)
          (when (not (equal point-before (point))) (throw 'terminate-complete t))) ))
    nil))

(defun dwim-tab-generator ( complete-functions )
  "dwim-tab-generator FUNCTIONS

   creates a function for the tab key hook that will:

   1. try contextual (global,local) DTRT functions at the
      point stopping if a function succeeds.

   2.A when following non-whitespace try completion functions
     B otherwise indent according to the mode.

   The context functions are shared globally, while the
   completion functions are bound to the generated function.
  "
  (lexical-let
    ((completion complete-functions))

    ;; If I create a un-interned symbol I think my problems will go away.
    (bind-eval-lambda "dwim-tab"
      (lambda ()
        "Complete if point is at end of a word, otherwise indent line."
        (interactive)

        ;; The contextual functions are tried first. The local
        ;; context overrides the global contextual functions

        ;; first try the tab context which should override the
        ;; general tab behavior only when the text or properties
        ;; essentially guarantee to DTRT
        (unless (or
                  (try-context-dwim dwim-tab-local-context)
                  (try-context-dwim dwim-tab-global-context))

          ;; FIXME: this regex match is fishy. It doesn't expand when the cursor
          ;;        follows the "-" character.

          (if (looking-at "\\>")
            ;; complete or indent when the cursor is positioned at the end of a word.
            (when (not (try-complete-dwim completion)) (ding))
            (indent-for-tab-command))) )) ))

(defun dwim-tab-localize ( &rest completion-functions )
  "dwim-tab-localize &COMPLETE
   install a dwim-tab into the local keymap that is a capture of dwim-tab-fallback
   and the COMPLETE functions at the time of installation. Changes in
   dwim-tab-global-context and dwim-tab-local-context will affect all tab bindings.
  "
  (local-set-key (kbd "<tab>") (apply 'dwim-tab-generator
                                 (seq-filter-nil (append completion-functions
                                                   (if (listp dwim-tab-fallback)
                                                     dwim-tab-fallback
                                                     (list dwim-tab-fallback)) ))
                                 )))

(provide 'dwim-tab)

;;; dwim-tab.el ends here
