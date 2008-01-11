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

(defun run-test-interp-token ( match-function )
  (lexical-let
    ((result (funcall match-function)))
    (message "TI match? %s AST %s" (if (car result) "yes" "no") (pp (cdr result)))
    ))

(defun run-test-token ()
  (interactive)
  (run-test-interp-token (test-interp-token token whitespace "[[:blank:]]+")))

(pp (macroexpand
      (test-interp-token token whitespace "[[:blank:]]")))

;;----------------------------------------------------------------------
;; test tokens
;;----------------------------------------------------------------------

;; eval this, then eval various tests.

(defun run-parser ()
  "run test-parser interactively for testing and debugging."
  (interactive)
  (lexical-let
    ((parse-result (test-parser (point))))

    (message "PROD match? %s"
      (if parse-result
        (format "Yes matched to: %s, AST: %s" (car parse-result) (pp (cdr parse-result))
        "No")
      ))
    ))

(parser-compile test-parser
  (token whitespace "[[:blank:]]+"))

(parser-compile test-parser
  (token whitespace "[[:blank:]]+" (lambda ( start stop ) (message "bar by far"))))

(parser-compile test-parser
  (token whitespace "[[:blank:]]+" bingo))

;;----------------------------------------------------------------------
;; test productions
;;----------------------------------------------------------------------

(parser-compile test-parser
  (token whitespace "[[:blank:]]+")
  (token word "[[:alpha:]]+"))

(parser-compile test-parser
  (and indented (token whitespace "[[:blank:]]+") (token word "[[:alpha:]]+"))
  (and inverted word whitespace))

fooo   bar

