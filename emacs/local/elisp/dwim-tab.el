;;----------------------------------------------------------------------
;; dwim-tab.el
;;----------------------------------------------------------------------
(eval-when-compile
  (require 'cl))

(defvar dwim-tab-register-expand nil
  "dwim-tab function for expanding registers")

(defvar dwim-tab-global-context nil
  "A list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defvar-local dwim-tab-local-context nil
  "A function, or list of functions ")

(defvar dwim-tab-indent 'indent-according-to-mode)

(defun dwim-tab-set-register-expand ( expander )
  (setq dwim-tab-register-expand expander))

(defun dwim-tab-stem-trigger ()
  (if (looking-at-p "\\>")
    t
    nil))

(defun dwim-tab-word-trigger ()
  (let
    (( at-point (thing-at-point 'word) ))

    (if (and (stringp at-point)
             (and (string-match "\\sw" at-point)
                  (string-match "\\sw" (char-to-string (char-after))) ))
      t
      nil) ))

(defun dwim-tab-make-expander ( context expander )
  (cons context expander))

(defun dwim-tab-expanders-by-regex ()
  (let
    (( all-expanders (append dwim-tab-local-context dwim-tab-global-context))
     ( relevant-expanders nil) )

    (mapc
      (lambda ( expander )
        (when (funcall (car expander))
          (setq relevant-expanders (cons (cdr expander) relevant-expanders))) )
      all-expanders)

    (if dwim-tab-register-expand
      (cons dwim-tab-register-expand relevant-expanders)
      relevant-expanders) ))

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

(defvar dwim-buffer-change-status nil)

(defun dwim-buffer-change-hook ( start end )
  (setq dwim-buffer-change-status t))

 (defun dwim-install-change-hook ()
   (setq dwim-buffer-change-status nil)
   (add-hook 'before-change-functions 'dwim-buffer-change-hook nil t))

 (defun dwim-remove-change-hook ()
   (remove-hook 'before-change-functions 'dwim-buffer-change-hook t))

(defun try-complete-dwim ()
  "try-complete-dwim COMPLETE

   Search through the COMPLETE function list for a expansion. This
   function catches 'terminate-complete.
  "
  (interactive)
  (let
    ((complete (dwim-tab-expanders-by-regex))
     (point-before (point))
     (success nil))

    (when complete
      (dwim-install-change-hook)

      (setq dwim-buffer-change-status nil)

      (while complete
        (funcall (car complete))

        (setq complete
          (if dwim-buffer-change-status
            (progn
              (setq success t)
              nil)
            (cdr complete))) )

      (dwim-remove-change-hook))

      success))

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

  (unless (try-complete-dwim)
    (funcall dwim-tab-indent)) )

(defun turn-on-dwim-tab ( &optional indent-function )
  (interactive)

  (if indent-function
    (progn
      (make-variable-buffer-local 'dwim-tab-indent)
      (setq dwim-tab-indent indent-function)))

  (local-set-key (kbd "TAB") 'dwim-tab-do-magic))

(provide 'dwim-tab)

;;; dwim-tab.el ends here
