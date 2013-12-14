;;----------------------------------------------------------------------
;; lex-cache.el - cache data lexically
;;
;; description:
;;
;; cache a piece of data lexically.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; cache expiration
;;----------------------------------------------------------------------

(defun lex-cache-create-timestamp ()
  (truncate (time-to-seconds (current-time))))

(defun lex-cache-timestamp-expired-p ( timestamp interval )
  (if (> (lex-cache-create-timestamp) (+ timestamp interval))
    t
    nil))

(defun lex-cache-need-update-p ( timestamp interval )
  (and timestamp (lex-cache-timestamp-expired-p timestamp interval)))

(defun lex-cache-minutes ( min )
  (* 60 min))

(defun lex-cache-bind ( symbol cache-fn )
   (fset symbol cache-fn)
   symbol)

(defun lex-cache-build ( builder interval )
  (lexical-let
    ((builder-fn builder)
     (data nil)
     (data-interval interval)
     (data-timestamp nil))

    (lambda ( &optional update-flag )
      (when (eq 'force update-flag)
        (setq data nil))

      (if update-flag
        (when (or (not data) (lex-cache-need-update-p data-timestamp data-interval))
          (setq
            data (funcall builder-fn)
            data-timestamp (lex-cache-create-timestamp))
          nil)
        (if (and data (not (lex-cache-need-update-p data-timestamp data-interval)))
          data
          (progn
            (setq
              data (funcall builder-fn)
              data-timestamp (lex-cache-create-timestamp))
            data)) )) ))

(defmacro lex-cache ( binding interval &rest body )
  `(lex-cache-bind ',binding
     (lex-cache-build
       ,@body
       ,interval)) )

(provide 'lex-cache)
