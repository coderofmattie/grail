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


;;----------------------------------------------------------------------
;; combination operators interpretation
;;----------------------------------------------------------------------

(pp (macroexpand
      (test-interp-token token whitespace "[[:blank:]]")))

;;----------------------------------------------------------------------
;; combination operators compilation
;;----------------------------------------------------------------------


(test-compile
  (token whitespace "[[:blank:]]+"))

(test-compile
  (token whitespace "[[:blank:]]*" (lambda ( start stop ) (message "bar by far"))))

(test-compile
  (token whitespace "[[:blank:]]*" bingo))

;; will need a test-comp-token that defines the match-table so that I can
;; test the token compiler independently.

(setq test-parser (parser-compile
                     (token whitespace "[[:blank:]]+")
                     (token word "[[:alpha:]]+")))

(setq test-parser (parser-compile
                    (token word "[[:alpha:]]+")))


(pp (macroexpand '(parser-compile
                     (token word "[[:alpha:]]+"))))


(setq foo (eval (lambda () (message "foo!"))))

(type-of foo)
(funcall foo)

(defun test-prod ()
  (let
    ;; create a symbol table to store compiled terminal and
    ;; non-terminal match functions
    ((match-table (make-vector parser-mtable-init-size 0)))
    (parser-compile-production 'start 'parser-or (parser-compile-definition '((token word "[[:alpha:]]+"))))
    ))

(setq test-parser (test-prod))

(pp test-parser)

(defun run-prod ()
  (interactive)
  (let*
    ((parser-position (cons (point) nil))
      (test-result (test-parser)))
    (message "PROD match? %s AST %s" (if (car test-result) "yes" "no") (pp (cdr test-result)))
    ))

(defun run-parser ()
  (interactive)
  (let
    ((test-result (funcall test-parser (point))))
    (message "TFC match? %s AST %s" (if (car test-result) "yes" "no") (pp (cdr test-result)))
    ))

(defun shit ()
  (interactive)
  (funcall foo (point)))
