;;----------------------------------------------------------------------
;; borg-repl.el
;;----------------------------------------------------------------------
(require 'custom-key)

(defun syntax-move/error-msg ( msg &rest info )
  (message "syntax-move Error: %s %s" msg
    (if info
      (concat "[ " (string-join (mapcar
                                 (lambda (x)
                                   (if (stringp x)
                                     x
                                     (pp-to-string x)))
                                 info) ", ") " ]")
      "")))

(defun syntax-move/bind-prev ( binding )
  (set (make-local-variable 'syntax-move/prev-binding) binding) )

(defun syntax-move/bind-next ( binding )
  (set (make-local-variable 'syntax-move/next-binding) binding) )

(defun syntax-move/bind-opening ( binding )
  (set (make-local-variable 'syntax-move/opening-binding) binding) )

(defun syntax-move/bind-closing ( binding )
  (set (make-local-variable 'syntax-move/closing-binding) binding) )

(defun syntax-move/bind-up-scope ( binding )
  (set (make-local-variable 'syntax-move/up-scope-binding) binding) )

(defun syntax-move/bind-down-scope ( binding )
  (set (make-local-variable 'syntax-move/down-scope-binding) binding) )

(defun syntax-move/bind-kill-expr ( binding )
  (set (make-local-variable 'syntax-move/kill-expr-binding) binding) )

(defun syntax-move/binding-wrapper ( binding )
  (if (commandp binding)
    (let
      (( current-prefix-arg nil ))

      (call-interactively binding))

    (funcall binding) ))

(defun syntax-move/prev ()
  "syntax-move/next

   move to the previous syntax object
  "
  (interactive)
  (if (boundp 'syntax-move/prev-binding)
    (syntax-move/binding-wrapper syntax-move/prev-binding)
    (syntax-move/error-msg "no syntax-move/prev binding in this buffer") ))

(defun syntax-move/next ()
  "syntax-move/next

   move to the next syntax object
  "
  (interactive)
  (if (boundp 'syntax-move/next-binding)
    (syntax-move/binding-wrapper syntax-move/next-binding)
    (syntax-move/error-msg "no syntax-move/next binding in this buffer") ))

(defun syntax-move/opening ()
  "syntax-move/opening

   move to the opening of the scope
  "
  (interactive)
  (if (boundp 'syntax-move/opening-binding)
    (syntax-move/binding-wrapper syntax-move/opening-binding)
    (syntax-move/error-msg "no syntax-move/opening binding in this buffer") ))

(defun syntax-move/closing ()
  "syntax-move/next

   move to the closing of the scope
  "
  (interactive)
  (if (boundp 'syntax-move/closing-binding)
    (syntax-move/binding-wrapper syntax-move/closing-binding)
    (syntax-move/error-msg "no syntax-move/closing binding in this buffer") ))

(defun syntax-move/up-scope ()
  "syntax-move/up-scope

   move to the top enclosing scope
  "
  (interactive)
  (if (boundp 'syntax-move/up-scope-binding)
    (syntax-move/binding-wrapper syntax-move/up-scope-binding)
    (syntax-move/error-msg "no syntax-move/up-scope binding in this buffer") ))

(defun syntax-move/down-scope ()
  "syntax-move/down-scope

   move to the bottom of the scope
  "
  (interactive)
  (if (boundp 'syntax-move/down-scope-binding)
    (syntax-move/binding-wrapper syntax-move/down-scope-binding)
    (syntax-move/error-msg "no syntax-move/down-scope binding in this buffer") ))

(defun syntax-move/kill-expr ()
  "syntax-move/kill-expr

   kill the expr by syntax.
  "
  (interactive)
  (if (boundp 'syntax-move/kill-expr-binding)
    (syntax-move/binding-wrapper syntax-move/kill-expr-binding)
    (syntax-move/error-msg "no syntax-move/kill-expr binding in this buffer") ))

(defun syntax-move/local-keybindings ()
  (interactive)

  (custom-key-set "syntax move" "M-C" nil
    ("b" . syntax-move/prev)
    ("f" . syntax-move/next)
    ("o" . syntax-move/opening)
    ("c" . syntax-move/closing)
    ("u" . syntax-move/up-scope)
    ("d" . syntax-move/down-scope)
    ("k" . syntax-move/kill-expr) ) )

(provide 'syntax-move)
