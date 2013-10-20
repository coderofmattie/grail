;;----------------------------------------------------------------------
;; lisp utilities
;;----------------------------------------------------------------------
(defun lisp-map ( fn list )
  (let
    ((transform (funcall fn (car list))))

    (if (not (eq nil (cdr list)))
      (cons transform (lisp-map fn (cdr list)))
      (cons transform (cdr list)) )))

(provide 'utilities)
