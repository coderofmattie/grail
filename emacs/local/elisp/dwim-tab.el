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

(defvar dwim-tab-register-expand nil
  "dwim-tab function for expanding registers")

(defvar dwim-tab-global-context nil
  "A list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defvar-local dwim-tab-local-context nil
  "A function, or list of functions ")

(defvar-local dwim-tab-local-indent 'indent-for-tab-command)

(defun dwim-tab-set-register-expand ( expander )
  (setq dwim-tab-register-expand expander))

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

(defun try-complete-dwim ()
  "try-complete-dwim COMPLETE

   Search through the COMPLETE function list for a expansion. This
   function catches 'terminate-complete.
  "
  (interactive)
  (let
    ((complete nil)
     (point-before (point)))

    (setq complete (append dwim-tab-global-context complete))
    (setq complete (append dwim-tab-local-context complete))

    (setq complete (nreverse complete))

    (when dwim-tab-register-expand
      (setq complete (cons dwim-tab-register-expand complete)))

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
    (unless (try-complete-dwim) (funcall dwim-tab-local-indent))
    (funcall dwim-tab-local-indent)) )

(defun turn-on-dwim-tab ( &optional indent-function )
  (interactive)

  (if (not (equal nil indent-function))
    (setq dwim-tab-local-indent indent-function))

  (local-set-key (kbd "TAB") 'dwim-tab-do-magic))

(provide 'dwim-tab)

;;; dwim-tab.el ends here
