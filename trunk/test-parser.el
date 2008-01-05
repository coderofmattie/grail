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

(defmacro test-parser ( &rest syntax )
  (lexical-let
    ((compile (catch 'syntax-error
                (parser-compile-token (cdr syntax)))))

    (if (stringp compile)
      (message "%s" compile)
      compile)
    ))

(pp (macroexpand
      (test-parser token whitespace "[[:blank:]]")))

(test-token 'whitespace "[[:blank:]]")
