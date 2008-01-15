;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;; Copyright (C) 2008 Mike Mattie
;; Description: Recursive Descent parser compiler implemented as a Macro.
;;----------------------------------------------------------------------

;; ->Summary.

;; parser.el aims to be a lightweight implementation of Recursive
;; Descent parsing. The parser-compile macro is given a symbol and a
;; grammar definition. It returns a compiled parser bound to the
;; function of the symbol.

;; -> Application.

;; Building or extracting a nested data structure by parsing static
;; text.

;; -> Comparison with Emacs parsing facilities.

;; Emacs supports parsing in two forms: Regular Expressions and tools
;; like Syntax Tables geared towards using the parse results to
;; annotate buffers with text-properties and overlays.

;; This Dynamic Programming approach works well for overlaying
;; functions that interpret the meaning of text in the buffer, such as
;; syntax highlighting, on a paradigm of unstructured text. The
;; analysis is preserved when the buffer is edited at a character
;; level.

;; When you need to build an interface that rests entirely on the
;; parse analysis to the degree that the user or program does not
;; modify or traverse the buffer at a character level, this tool
;; simplifies construction of a nested data structure that maps tokens
;; to beginning and ending positions of the match.

;; -> Related Works

;; * CEDET.

;; CEDET appears to be a parser generator implemented on-top of a CLOS
;; emulation. This tool-set has developed towards the problem of
;; analyzing source code as a project which is bloated for simpler
;; requirements.

;; CEDET is likely to offer much higher performance than an un-optimized
;; recursive descent parser. If backtracking needs to be optimized
;; something like CEDET is a better choice than parser-compile.

;; ->Concept

;; A parser compiler that combines lexical (regex) analysis with recursive
;; descent parsing compiled by macro expansion.

;; The concept started with the idea to build a parser as nested cond
;; forms. The matching would go in the predicate part and the action
;; in the body.

;; This essential simplicity is still present in the design but the
;; implementation is now more complex with Match Functions replacing
;; the cond clause form. Match Functions allow previous definitions of
;; a token or rule to be referenced later in later rules. Backtracking
;; was another essential complexity once the and non-terminal was
;; added.

;; ->Requirements

;; 1. Easy to use for trivial problems, powerful enough to solve
;;    most of them.

;; 2. Data structures are orthogonal to the parser and not hard-wired
;;    in. Tokens can perform user defined functions or construct custom
;;    data structures with the beginning and ending positions of the
;;    match in the input.
;;
;;    This allows the user to choose between positions, markers, overlays
;;    according to the requirements.

;; ->Characteristics

;; parser-compile produces a no frills recursive descent parser.

;; ->Terminology

;; My reference for parsing terminology is the Dragon Book:

;; Compilers
;; Principles,Techniques,and Tools
;; Alfred V.Aho, Ravi Sethi, Jeffrey D.Ullman
;; 1986, Addison Wesley

;; ->TODO

;; 1. define list where Matches can be defined without inserting a
;;    match at the definition point [implemented, but not tested]

;; 2. optional matching with ? for Match Function references. [easy]

;; 3. Canonical tree walk implemented as parser-ast-node.

(require 'cl)

;; not defined in Gnu Emacs evidently.

(defmacro define-error ( symbol message &rest isa-list )
  "define a error symbol with a isa list and a error message"
  `(progn
     (put ',symbol
       'error-conditions (append '(error ,symbol) ',isa-list))
     (put ',symbol 'error-message ,message) ))

;; A recursive macro expansion would be nice for creating a hierarchy.
(define-error parser-compile-error  "parser error")
  (define-error parser-syntactic-error  "syntactic error" parser-compile-error)
  (define-error parser-semantic-error   "semantic error" parser-compile-error)

;;----------------------------------------------------------------------
;; Backtracking.
;;----------------------------------------------------------------------

;; parser-position

;; The position of the parser in the input stream is maintained as a
;; stack of indexes into the buffer. As matching progresses the top of
;; the stack (car) is updated. The push and pop operations copy a
;; position to a next or previous stack position. backtrack discards
;; the top of the stack.

(defsubst parser-pos () ;; tested
  "Return the current position of the parser in the buffer"
  (car parser-position))

(defun parser-push ()
  "Copy the parser position to a new stack level so the parser can backtrack when necessary."
  (push (parser-pos) parser-position))

(defun parser-pop ()
  "Copy the parser position to a previous stack level When the possibility of a backtrack
   has been eliminated by matching."
  (let
    ((current (pop parser-position)))
    (setcar parser-position current) ))

(defun parser-backtrack ()
  "Restore the previous parser position by discarding the top of the parser-position stack."
  (pop parser-position)
  (goto-char (parser-pos)))

(defun parser-advance ( consumed )
  "Advance the input position of the parser to the next un-matched character: input consumed + 1."

  (if (> consumed 0)
    (lexical-let
      ((pos (+ 1 consumed (parser-pos))))
      (progn
        (setcar parser-position pos)
        (goto-char pos))) ))

(defun parser-consumed ()
  "The number of input characters consumed by the token's match in the input."
  (- (match-end 0) (match-beginning 0)))

;;----------------------------------------------------------------------
;; Match Result
;;----------------------------------------------------------------------

;; The parser functions have a standard structure for returning the
;; result of a Match Function.

;; nil | (parser . AST ).

;; nil indicates failure to match.

;; A successful match has a parser and AST parts. The parser is
;; the ending position of the match relative to the starting
;; match position.

;; The AST part is created by a token action handler or a combination
;; operator like and.

;; token ( match-symbol . ( start . end ) | "user value" | "user function return value")
;; and   ( match-symbol . "a list of Match Result AST parts" )

;; This consistent return value structure allows token/and/or
;; match-functions to nest arbitrarily [except that tokens are
;; strictly leafs].

;; My rationale for using inline functions is that I am essentially naming
;; the car and cdr of my cons cell for readability.

(defsubst parser-make-match ( consumed data )
  "create a match result from the input consumed by the match, and the match data."
  (cons consumed data))

(defsubst parser-match-consumed ( match-result )
  "return the input consumed by the match"
  (car match-result))

(defsubst parser-match-data ( match-result )
  "return the data of the match"
  (cdr match-result))

(defsubst parser-make-match-data ( name data )
  (cons name data))

;;----------------------------------------------------------------------
;; Parser Debugging
;;----------------------------------------------------------------------

;; A tracing facility that can be selectively turned on and off for
;; productions. When tracing is turned on the result of all matches
;; attempted are printed to Message.

;; A report of how the compiled parser matched the input stream is
;; vital to developing a working grammar.

(defun parser-match-trace ( match-func match-result )
  "trace a match"
  (if (and (boundp 'parser-trace-flag) (eq t parser-trace-flag))
    (message "[Parser Trace] %s at: %s match: %s"
      (symbol-name match-func)
      (parser-pos)
      (pp match-result)) ))

(defun parser-trace-p ( production )
  "return a trace flag"
  (message "got here !")
  (catch 'abort
    (unless (and (boundp 'parser-trace) (listp parser-trace)) (throw 'abort nil))

    (lexical-let
      ((toggle (apply 'or (mapcar (lambda ( trace-on )
                                    (if (eq production (car trace-on))
                                      (cdr trace-on)))
                            parser-trace))))
      (if toggle
        (cons t toggle)
        (cons nil nil)
        )) ))

(defmacro parser-trace-on ( production &rest code )
  `(lexical-let*
    ((code-func (lambda () ,@code))
      (trace-p (parser-trace-p ',production))
      (trace-toggle (cdr trace-p)) )

     (if (and
           (car trace-p)
           (and
             (boundp 'parser-trace-flag)
             (not (eq parser-trace-flag trace-toggle)) ))
       (let
         ((parse-trace-flag trace-toggle))
         (funcall code-func))
       (funcall code-func)) ))

;; creative debugging idea. A flag could indicate at compilation to
;; ignore all of the token handlers and instead create overlays with
;; the name of the token as a mouse-over. This could also be toggled
;; at run-time actually.

;;----------------------------------------------------------------------
;; Combination Operators
;;----------------------------------------------------------------------

;; The parser uses two combination operators: parser-and, parser-or as
;; nodes in the parser tree. Significantly parser-and can backtrack
;; and parser-or never does.

;; and/or have the same essential meaning as the lisp and/or forms
;; with two specializations. Both functions treat their argument lists
;; as a list of Match Functions. Also the parser-and function returns
;; a production consisting of the AST parts of the Match Results,
;; instead of returning the last Match Result.

(defun parser-or ( &rest match-list )
  "Combine Match Functions by or ; the first successful match is returned.
   nil is returned if no matches are found"
  (catch 'match
    (dolist (match match-list)
      (parser-trace-on match
        (lexical-let
          ((production (funcall match)))

          (parser-match-trace match production)

          (if production
            (progn
              (parser-advance (parser-match-consumed production))
              (throw 'match (parser-make-match 0 (parser-match-data production))))
            )))) ))

(defun parser-and ( &rest match-list )
  "combine the matches with and. all of the match objects must return non-nil
   in the parser part or the parser will backtrack and return nil."
  (parser-push)

  (lexical-let
    ((production (catch 'backtrack
                   ;; we want to gather all the matches, so mapcar across the match objects.
                   (mapcar
                     (lambda (match)
                       (parser-trace-on match
                         (lexical-let
                           ((production (funcall match)))

                           (parser-match-trace match production)

                           (if production
                             (progn
                               (parser-advance (parser-match-consumed production))
                               (parser-match-data production))

                             (throw 'backtrack nil)) )))
                     match-list)) ))
    (if production
      (progn
        (parser-pop)
        (parser-make-match 0 production)) ;; would be nice to filter optional matches here.
      (progn
        (parser-backtrack)
        nil)) ))

;;----------------------------------------------------------------------
;; compiler diagnostics
;;----------------------------------------------------------------------

;; construct meaningful compiler error messages in the
;; "expected: foo got: bar" form.

(defun parser-expr-diagnostic ( form ) ;; tested
  (format "type(%s) %s" (symbol-name (type-of form)) (pp (eval form))))

(defmacro parser-diagnostic ( form from expected ) ;; tested
  "syntax: (parser-diagnostic form from expected)

   Where form is the expr received, from is the component issuing the diagnostic,
   and expected is a message describing what the component expected"
  `(concat (format "[%s] expected: " ,from)  ,expected " not: " ,(parser-expr-diagnostic form)))

;;----------------------------------------------------------------------
;; Match Functions
;;----------------------------------------------------------------------

;; Match functions are lambdas that take no arguments and return Match
;; Result values. They assume that the compiled parser has scoped a
;; parser-position stack to determine the parsing position in input.

;; match-table is objarray created at macro scope, and does not appear
;; in the compiled form.

(defun parser-match-function ( identifier &optional definition )
  "Retrieve or define a Match Function."
  (lexical-let*
    ((id (symbol-name identifier))
      (lookup (intern-soft id match-table)))

    (if lookup
      (if (eq definition nil)
        lookup
        (signal 'parser-semantic-error (format "illegal redefinition of Match Function %s" id)))

      (progn
        (fset (intern id match-table) (eval definition))
        (intern id match-table))
      )))

;;----------------------------------------------------------------------
;; tokens
;;----------------------------------------------------------------------

;; EXPERIMENT: allow other functions to be used for token matching
;;             other than looking-at, such as re-search.

(defun parser-build-token ( identifier ) ;; tested
  "parser-make-token is a built-in constructor that records the analysis
   and the location of the text (id . (begin . end))"
  (parser-make-match-data identifier (cons (match-beginning 0) (match-end 0))))

(defun parser-make-anon-func ( name sexp ) ;; tested
  "bind an un-evaluated anonymous function to an un-interned symbol"
  (let
    ((anon-func (make-symbol name)))
    (fset anon-func (eval sexp))
    anon-func))

;; the token interpreter was split into two functions to isolate the
;; flexibility of tokens (user functions or return values for
;; constructing AST) from the hard-wired parser part.

(defun parser-interp-token-action ( identifier constructor ) ;; tested
  "Translate the AST constructor part of a token definition into Elisp."

  (unless (symbolp identifier)
    (signal 'parser-syntactic-error
      (parser-diagnostic identifier
        "parser token"
        "identifier: An unbound symbol used as an identifier"
        )))

  ;; Warning: this code is very touchy, double quotation of AST
  ;; symbols is required. the saving grace is that the symbols don't
  ;; have a variable value bound so it fails noisily when the
  ;; quotation is incorrect.
  (cond
    ((eq nil constructor)    `(parser-build-token '',identifier))
    ((listp constructor)     `(,(parser-make-anon-func "parser-user-handler" constructor)
                                (match-beginning 0) (match-end 0)))
    ((functionp constructor) `(,constructor (match-beginning 0) (match-end 0)))
    ((symbolp constructor)   `(quote ',constructor))

    ;; all other constructor types are un-handled.
    ((signal 'parser-syntactic-error
       (parser-diagnostic identifier
         "parser token: identifier" "A symbol"))))
  )

(defun parser-interp-token ( syntax ) ;; tested
  "Translate a token definition into a Match Function.

   The matching part is hard-wired into the function, while the
   AST part which contains a great degree of flexibility is
   translated by parser-interp-token-action"

  (lexical-let
    ((identifier  (car syntax))
     (regex       (cadr syntax))    ;; eval ? many regexs are stored in variables.
     (constructor (caddr syntax)))

    `(lambda ()
       (if (looking-at ,regex)
         (parser-make-match (parser-consumed) ,(parser-interp-token-action identifier constructor))
         nil
         ))
    ))

(defun parser-compile-token ( syntax ) ;; tested
  "Compile a token into a Match Function."
  (parser-match-function (car syntax) (parser-interp-token syntax)))

;;----------------------------------------------------------------------
;; rules
;;----------------------------------------------------------------------

;; rules are non-terminals, with a left side or identifier, and a
;; right side containing matches that recognize a production of the
;; rule.

(defun parser-rule-right ( rule )
  "Translate a match in the Right Side of the rule into a
   compiled Match Function by retrieving the Match Function or
   recursively interpreting the grammar definition."

  (cond
    ((listp rule) (parser-compile-definition rule))
    ((symbolp rule) (parser-match-function rule))

    (signal 'parser-syntactic-error
      (parser-daignostic rule
        "Rule Right interpreter"
        "expected a grammar list e.g: token,and,or ; or a symbol as a rule or token reference"))
    ))

;; both parser-compile-anon-rule and parser-rule-left are the latest
;; point at which we can catch nils in the parser tree so they must
;; both use the list-filter-nil function.

(defun list-filter-nil ( list )
  "filter nil symbols from a list"
  (if (consp list)
    (lexical-let
      ((head (car list)))

      (if (eq head 'nil)
        (list-filter-nil (cdr list))
        (cons head (list-filter-nil (cdr list)))
        ))
    nil
    ))

(defun parser-compile-anon-rule ( combine-function prod-right )
  "Compile a Match Function out of an anonymous rule which is
   fairly simple since we do not need to construct a match, only
   pass it through."
  (parser-make-anon-func (symbol-name combine-function)
    `(lambda ()
       (apply ',combine-function ',(list-filter-nil (mapcar 'parser-rule-right prod-right))))
    ))

(defun parser-rule-left ( prod-left combine-operator prod-right )
  "Compile a Match Function from a named rule."

  (unless (symbolp prod-left)
    (parser-diagnostic prod-left
      "Rule Left interpreter"
      "left side of the production or an identifier"))

  (lexical-let
    ((matchf-list (list-filter-nil prod-right)))

    (if matchf-list
      (parser-match-function prod-left
        `(lambda ()
           (lexical-let
             ((production (apply ',combine-operator ',matchf-list)))
             (if production
               (parser-make-match
                 (parser-match-consumed production)
                 (parser-make-match-data '',prod-left (parser-match-data production))))
             )))
      (progn
        (message "parser-compile Warning! named rule %s deleted with no matches in rule"
          (symbol-name prod-left))
        nil))
    ))

(defun parser-compile-named-rule ( combine-function prod-right )
  "compile a named rule"
  (let
    ((production (car prod-right))
      (rules (cdr prod-right)))

    (unless (symbolp production)
      (signal 'parser-syntactic-error
        (parser-diagnostic production
          "Rule interpreter"
          "a symbol to name the production")))

    (parser-rule-left production combine-function (mapcar 'parser-rule-right rules))
    ))

;;----------------------------------------------------------------------
;; grammar definition.
;;----------------------------------------------------------------------

(defun parser-compile-definition ( term )
  "parser-compile-definition is the recursive heart of the compiler."
  (unless (listp term)
    (signal 'parser-syntactic-error
      (parser-diagnostic term
        "parser definition"
        "expected a definition of token|or|and|define")))

  (lexical-let
    ((keyword (car term))
      (syntax (cdr term)))

    (if (listp keyword)
      (parser-compile-definition keyword)

      (cond
        ((eq keyword 'token)  (parser-compile-token syntax))
        ((eq keyword 'or)     (parser-compile-anon-rule 'parser-or syntax))
        ((eq keyword 'and)    (parser-compile-named-rule 'parser-and syntax))

        ((eq keyword 'define)
          ;; define discards the match functions as a return value so
          ;; tokens and rules can be defined before they are used.
          (mapcar 'parser-rule-right syntax)
          nil)

        ((signal 'parser-syntactic-error
           (parser-diagnostic term
             "parser definition"
             "definition keyword token|or|and|define")))
        ))
    ))

(defvar parser-mtable-init-size 13
  "initial size of the match-table objarray for storing match functions. the value
   was chosen based on the recommendation of prime numbers for good hashing.")

(defmacro parser-compile ( parser &rest definition )
  "compile a LL parser from the given grammar."
  (let
    ;; create a symbol table to store compiled terminal and
    ;; non-terminal match functions
    ((match-table (make-vector parser-mtable-init-size 0)))

    (condition-case diagnostic
      (progn
        (fset parser
          (eval
            `(lambda ( start-pos )
               (let
                 ((parser-position (cons start-pos nil))) ;; initialize the backtrack stack
                 (save-excursion
                   (goto-char start-pos)
                   ;; note that the start symbol of the grammar is built in as an or combination
                   ;; of the top-level definitions.
                   (lexical-let
                     ((parse (,(parser-rule-left 'start 'parser-or
                                 (mapcar 'parser-compile-definition definition))
                               )))
                     (if parse
                       ;; if we have a production return the position at which the
                       ;; parser stopped along with the AST.
                       (parser-make-match (parser-pos) (parser-match-data parse))
                       nil))
                   )))
            ))
        t)

      (parser-syntactic-error
        (progn
          (message "parser-compile Syntax Error %s" diagnostic)
          nil
          ))
      (parser-semantic-error
        (progn
          (message "parser-compile invalid statement %s" diagnostic)
        ))
      )))

(defun parser-interactive (parser)
  "run test-parser interactively for testing and debugging."
  (interactive "SParser? ")
  (lexical-let
    ((parse-result (funcall parser (point))))

    (message "PROD match? %s"
      (if parse-result
        (format "Yes matched to: %s, AST: %s" (car parse-result) (pp (cdr parse-result))
        "No")
      ))
    ))

(provide 'parser)
