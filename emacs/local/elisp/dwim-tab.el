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

(defvar dwim-tab-fallback-context nil
  "A fallback completion function which defaults to hippie expand.
   set dwim-tab-fallback to a function, or a list of
   functions. When more than one function is specified the
   functions will be executed in order until one or none returns
   non-nil.")

(defvar-local dwim-tab-local-context nil
  "A function, or list of functions ")

(defvar-local dwim-tab-local-ident nil)

(defun dwim-tab-localize-context ( &rest locals )
  "dwim-tab-local-context function-list

   Add a context function to the Tab hook. If the function detects that
   the point is within it's context, or turf then it should DTRT and
   return non-nil.
  "
  (apply 'add-to-list 'dwim-tab-local-context locals))

(defun dwim-tab-globalize-context ( &rest globals )
  "dwim-tab-globalize-context function

   Add a context function to the Tab hook. If the function detects that
   the point is within it's context, or turf then it should DTRT and
   return non-nil.
  "
  (apply 'add-to-list 'dwim-tab-global-context globals))

(defun dwim-tab-fallack-context ( &rest fallbacks  )
  "dwim-tab-fallback-context function

   Add a context function to the Tab hook. If the function detects that
   the point is within it's context, or turf then it should DTRT and
   return non-nil.
  "
  (apply 'add-to-list 'dwim-tab-fallback-context fallbacks))

(defun try-complete-dwim ()
  "try-complete-dwim COMPLETE

   Search through the COMPLETE function list for a expansion. This
   function catches 'terminate-complete.
  "
  (interactive)
  (let
    ((complete nil)
     (point-before (point)))

    (setq complete (append dwim-tab-fallback-context complete))
    (setq complete (append dwim-tab-global-context complete))
    (setq complete (append dwim-tab-local-context complete))

    (setq complete (nreverse complete))

    (catch 'terminate-complete
      (unless complete
        (throw 'terminate-complete nil))

        (dolist (fn complete)
          (funcall fn)

          (unless (equal point-before (point))
            (throw 'terminate-complete t)))
      nil) ))

(defun dwim-tab-do-magic ()
  "dwim-tab-do-magic FUNCTIONS

   1. try contextual (global,local) DTRT functions at the
      point stopping if a function succeeds.

   2.A when following non-whitespace try completion functions
     B otherwise indent according to the mode.

   The context functions are shared globally, while the
   completion functions are bound to the generated function.
  "
  (interactive)
  (if (looking-at "\\>")
    (unless (try-complete-dwim) (dwim-tab-local-indent))
    (dwim-tab-local-indent) ))

(defun turn-on-dwim-tab ( &optional indent-function )
  (interactive)

  (if indent-function
    (setq dwim-tab-local-indent (or indent-function 'indent-for-tab-command)))

  (local-set-key (kbd "<tab>") 'dwim-tab-do-magic) )

(provide 'dwim-tab)

;;; dwim-tab.el ends here
