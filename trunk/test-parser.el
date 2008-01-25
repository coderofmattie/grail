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

(parser-production-function

(parser-compile test-parser
  (token whitespace "[[:blank:]]+")
  (token word "[[:alpha:]]+"))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+") (token whitespace "[[:blank:]]+")))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+") (token whitespace "[[:blank:]]+")))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+" parser-token-string) (? (token whitespace "[[:blank:]]+"))))

parser foo bar baz||

(parser-compile test-parser
  (define
    (name bingo (or
                  (token whitespace "[[:blank:]]+")
                  (token word "[[:alpha:]]+"))))
  bingo)

(parser-trace-list test-trace
  (whitespace t)
  (word t)
  (start t))

(parser-compile test-parser
  (define
    (name foo (or
                (token whitespace "[[:blank:]]+")
                (token word "[[:alpha:]]+"))))
  foo)

(parser-compile test-parser
  (and indented (token whitespace "[[:blank:]]+") (token word "[[:alpha:]]+"))
  (and inverted word whitespace))

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

foo bar baz

;;----------------------------------------------------------------------
;; experimental
;;----------------------------------------------------------------------

now create a spiffy function that walks the AST tree for you.

something like start/indented/whitespace
or start/indented

this should lay the grounds for verifying wether the AST is generated as expected,
with parser-walk defining a canonical traversal.
