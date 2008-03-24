;;----------------------------------------------------------------------
;; mattie-elisp.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; Description: basic elisp programming tools.
;;----------------------------------------------------------------------

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

;; much like easy-mmode-define-keymap macro but with a little more
;; juice doing the defvar part as well.

(defmacro def-sparse-map ( symbol docstring &rest keys )
  "make it easy to define a keymap give the symbol, a docstring, followed by
   the usual (key 'symbol) lists."
  `(defvar ,symbol
     (let
       ((map (make-sparse-keymap)))
       ,@(mapcar (lambda (binding)
                   (list 'define-key 'map (car binding) (cadr binding))) keys)
       map)
     ,docstring))

(defun mode-overlay-at-point-p ( mode-symbol )
  "determine if the point is in a flyspell overlay. given a overlay list
   which may be nil, translate via predicate into boolean values which
   are then evaluated by or."
  (interactive)
  (let
    ((overlay-list (overlays-at (point))))

    (when overlay-list
      (eval (cons 'or
              (mapcar
                (lambda ( overlay )
                  (if (overlay-get overlay mode-symbol) t)) overlay-list)))) ))

(require 'cm-compat)
(require 'cm-list)
(require 'cm-lisp)
(require 'cm-string)

(provide 'mattie-elisp)
