;;----------------------------------------------------------------------
;; diagnostics
;;----------------------------------------------------------------------

;; test the diagnostics of the parser. I think some sort of eval may be necessary
;; in diagnostics.

;; see what the expansion looks like
(pp (macroexpand
      (parser-diagnostic `(foo bar)
        "test"
        "the right thing")))

;; see what it looks like when handled by message.
(message "%s"
      (parser-diagnostic `(foo bar)
        "test"
        "the right thing"))))

;;----------------------------------------------------------------------
;; token interp phase
;;----------------------------------------------------------------------

(defmacro test-interp-token ( &rest syntax )
  (lexical-let
    ((compile (catch 'syntax-error
                (parser-interp-token (cdr syntax)))))

    (if (stringp compile)
      (message "%s" compile)
      compile)
    ))

(pp (macroexpand
      (test-interp-token token whitespace "[[:blank:]]")))

(defun run-test-interp-token ( match-function )
  (lexical-let
    ((result (funcall match-function)))
    (message "TI match? %s AST %s" (if (car result) "yes" "no") (pp (cdr result)))
    ))

(defun run-test-token ()
  (interactive)
  (run-test-interp-token (test-interp-token token whitespace "[[:blank:]]+")))

;;----------------------------------------------------------------------
;; token compile phase
;;----------------------------------------------------------------------

;; this is the top-level of the compiler without the recursion and other
;; complexity to isolate a specific component for testing.
(defmacro test-compile ( test-syntax )
  (let
    ((match-table (make-vector parser-mtable-init-size 0)))

    `(defun run-test-token ()
       (interactive)
       (let*
         ((parser-position (cons (point) nil)) ;; initialize parser-position to the cursor.
           (test-match ,(parser-compile-token (cdr test-syntax)))
           (test-result (funcall test-match)))

         (message "TC match? %s AST %s" (if (car test-result) "yes" "no") (pp (cdr test-result)))
         ))
  ))


(test-compile
  (token whitespace "[[:blank:]]*"))

;; will need a test-comp-token that defines the match-table so that I can
;; test the token compiler independently.

(parser-compile
  (token whitespace "[[:blank:]]*")
  (token word "[[:alpha:]]+")
  (term whitespace? word whitespace?))
