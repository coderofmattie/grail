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

(defmacro test-compile-token ( &rest syntax )
  (lexical-let
    ((compile (catch 'syntax-error
                (parser-compile-token (cdr syntax)))))

    (if (stringp compile)
      (message "%s" compile)
      compile)
    ))

(pp (macroexpand
      (test-compile-token token whitespace "[[:blank:]]")))

(defun run-test-token ( match-function )
  (lexical-let
    ((result (funcall match-function)))
    (message "match? %s AST %s" (if (car result) "yes" "no") (pp (cdr result)))
    ))

(defun run-test ()
  (interactive)
  (let
    (result (funcall (test-compile-token token whitespace "[[:blank:]]")))
    (message "%s" (pp result))))

(defun run-test ()
  (interactive)
  (run-test-token (test-compile-token token whitespace "[[:blank:]]+")))

(match-fun 
(test-token 'whitespace "[[:blank:]]")
