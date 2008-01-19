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

;; parser-compile produces a no frills LL Recursive Descent parser.

;; ->Terminology

;; My reference for parsing terminology is the Dragon Book:

;; Compilers
;; Principles,Techniques,and Tools
;; Alfred V.Aho, Ravi Sethi, Jeffrey D.Ullman
;; 1986, Addison Wesley

;; ->TODO

;; 2. optional matching with ? for Match Function references. [easy]
;;    other meta-symbols to consider would be * for kleene closure
;;    and a bounded closure like {digit}

;; 3. Canonical tree walk implemented as parser-ast-node.

;; 4. re-write left recursion. This is essential to make grammars
;;    easy to write while avoiding infinite recursion problems.

;;    This will likely be a non-trivial hack requiring some extensive
;;    modifications to the parser. It would be nice if the surgery
;;    could be largely contained to parser-match-function.

(require 'cl)
(require 'mattie-elisp) ;; define-error, make-anon-func, list-filter-nil

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

(defsubst parser-pos ()
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
  "Advance the input position of the parser to the next un-matched character."

  (if (> consumed 0)
    (lexical-let
      ((pos (+ consumed (parser-pos))))
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
;; Parser Tracing
;;----------------------------------------------------------------------

;; A tracing facility that can be selectively turned on and off for
;; productions. When tracing is turned on the result of all matches
;; attempted are printed to a buffer, or the Message buffer.

;; A report of how the compiled parser matched the input stream is
;; vital to developing a working grammar.

(defun parser-trace-message ( format &rest args )
  "like message but prints to a trace buffer instead of the Message buffer."
  (with-current-buffer parser-trace-buffer
    (goto-char (point-max))
    (insert (apply 'format format args))))

(defun parser-trace-match ( match-func match-result )
  "Trace the current match if the parser-trace-flag is bound to t"
  (if (and (boundp 'parser-trace-flag) (eq t parser-trace-flag))
    (funcall
      (if (boundp 'parser-trace-buffer)
        'parser-trace-message
        'message)

      "%s at: %d match: %s"
      (symbol-name match-func)
      (parser-pos)
      (pp-to-string match-result)) ))

(defun parser-trace-p ( production )
  "Given a Match Function determine if parser-trace-flag should
   be set. The parser-trace list is scanned for a symbol match.
   The return value is a cons of a boolean indicating whether to
   set the flag, and the value of the flag.

   The parser-trace list is created by the macro parser-trace-list
   in utilities."

  (catch 'abort
    (unless (and (boundp 'parser-trace) (listp parser-trace)) (throw 'abort nil))

    (lexical-let
      ((toggle (eval (cons 'or (mapcar (lambda ( trace-on )
                                         ;; eq comparison of symbols does not work. A string
                                         ;; comparison is used for matching.
                                         (if (equal (symbol-name production) (car trace-on))
                                           (cdr trace-on)))
                                 parser-trace) ))))
      ;; a cons cell is returned so that a false value for the trace flag can be returned
      ;; without negating the truth value of the predicate itself.
      (if toggle
        (cons t toggle)
        (cons nil nil)
        )) ))

(defmacro parser-trace-on ( production &rest code )
  "parser-trace-on takes production and a code block code. If the production
   is on the parser-trace list a parser-trace-flag dynamically scoped is
   bound to the boolean toggle for tracing that production."

  ;; Using the dynamic scoping of let during the execution of the
  ;; compiled parser to scope parser-trace-flag gives tracing behavior
  ;; that precisely matches the execution of the parser.

  `(lexical-let*
    ((code-func (lambda () ,@code))
      (trace-p (parser-trace-p ,production))
      (trace-toggle (cdr trace-p)) )

     (if (and
           (car trace-p)

           ;; This expression attempts to minimize duplicate binding
           ;; of parser-trace-flag. If there are flaws in the tracing
           ;; behavior stemming from this expression it should be
           ;; removed entirely.
           (or
             (not (boundp 'parser-trace-flag))
             (not (eq parser-trace-flag trace-toggle))))
       (let
         ((parser-trace-flag trace-toggle))
         (funcall code-func))

       (funcall code-func)) ))

;;----------------------------------------------------------------------
;; Combination Operators
;;----------------------------------------------------------------------

;; The parser uses two combination operators: parser-and, parser-or as
;; nodes in the parser tree. Significantly parser-and can backtrack
;; and parser-or never does.

;; and/or have the same essential meaning as the lisp and/or forms
;; with two specializations. Both functions treat their argument lists
;; as a list of Match Functions.

;; parser-and is a named rule that returns Match Result lists.

;; parser-or is an anonymous production, it has no identity and passes
;; it's nested matches on transparently.

(defun parser-or ( &rest match-list )
  "Combine Match Functions by or ; the first successful match is returned.
   nil is returned if no matches are found"
  (catch 'match
    (dolist (match match-list)
      (parser-trace-on match
        (lexical-let
          ((production (funcall match)))

          (parser-trace-match match production)

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

                           (parser-trace-match match production)

                           (if production
                             (progn
                               (parser-advance (parser-match-consumed production))
                               (parser-match-data production))

                             (throw 'backtrack nil)) )))
                     match-list)) ))
    (if production
      (progn
        (parser-pop)
        ;; TODO: Any optional matches with 0 input characters consumed, or
        ;; nil AST could be filtered here.
        (parser-make-match 0 production))
      (progn
        (parser-backtrack)
        nil)) ))

;;----------------------------------------------------------------------
;; compiler diagnostics
;;----------------------------------------------------------------------

;; construct meaningful compiler error messages in the
;; "expected: foo got: bar" form.

(defun parser-expr-diagnostic ( form )
  (format "type(%s) %s" (symbol-name (type-of form)) (pp (eval form))))

(defmacro parser-diagnostic ( form from expected )
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
  "Retrieve or define a Match Function. the name of the production is required,
   if it is an anonymous production use parser-make-anon-func. For retrieval
   specify the symbol only, for definition pass the un-evaluated lambda expression
   as well."
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

(defun parser-token-match-range ( data-type capture )
  "Return the bounds of the capture from match-{beg,end} with the
   upper bound adjusted by decrement to inclusive. The type
   returned is chosen with a quoted type constructor symbol like
   cons or list."
  (eval `(,data-type (match-beginning capture) (- (match-end capture) 1)) ))

(defun parser-token-match-data ( identifier capture )
  "Construct the data of a token's Match Result combining the production identifier
   and the bounds of the matching input inclusive: (id . (begin . end))"
  (parser-make-match-data identifier (parser-token-match-range 'cons capture)))

;; The token part of the grammar definition contains a great deal of flexibility
;; or construction options for tokens.

;; parser-interp-token-action builds the constructor part of a token, while
;; parser-interp-token focuses on constructing a Match Function for the token.

(defun parser-interp-token-action ( identifier constructor )
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
    ((eq nil constructor)    `(parser-token-match-data '',identifier 0))
    ((listp constructor)     `(apply ',(make-anon-func "parser-user-handler" constructor)
                                (parser-token-match-range 'list 0)))
    ((functionp constructor) `(apply ',constructor (parser-token-match-range 'list 0)))
    ((numberp constructor)   `(parser-token-match-data '',identifier constructor))
    ((symbolp constructor)   `(quote ',constructor))

    ;; all other constructor types are un-handled.
    ((signal 'parser-syntactic-error
       (parser-diagnostic identifier
         "parser token: identifier" "A symbol"))))
  )

;; Experiment: allow other functions to be used for token matching
;;             other than looking-at, such as re-search.

(defun parser-interp-token ( syntax )
  "Translate a token definition into a Match Function."
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

;;----------------------------------------------------------------------
;; production right side evaluation
;;----------------------------------------------------------------------

;; Production right side evaluation is introduced before parser compilation
;; so that the recursion of evaluating a grammar statement can be inserted
;; into the parser compilation functions.

;; This simplifies the top level statement compilation by rendering the
;; recursive part of the statement an opaque object fed to the
;; parser compiler.

(defun parser-eval-rule-right ( rule )
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

;; both parser-compile-anon-rule and parser-compile-bound-rule are the
;; latest point at which we can catch nils in the parser tree so they
;; must both use the list-filter-nil function.

(defun parser-rule-right ( nil-warning rule-list )
  (lexical-let
    ((matchf-list (list-filter-nil (mapcar 'parser-eval-rule-right rule-list))))

    (if matchf-list
      matchf-list
      (progn
        (message "%s" nil-warning)
        (throw 'no-matches nil)))
    ))

;;----------------------------------------------------------------------
;; parser compiler
;;----------------------------------------------------------------------

;; the parser compiler part consists of two compilers and two
;; function generators.

;; The two compilers create operators or productions: operators are
;; compiled to un-interned symbols, production symbols are defined and
;; queried by parser-match-function.

;; inserting production symbols into the parse tree is implemented by
;; parser-production-statement, production at this phase of
;; compilation means a Match Function that can be queried.

;; the generators either produce a simple or compound rule function.
;; a simple rule evaluates a Match Function list with an operator.
;; Compound rule functions supply a transform function to modify the
;; parse tree produced by the operator and Match Function list.

;; These two sets of functions are combined with a functional
;; composition style to obtain N x N combination with N + N
;; declarations.

;; Generators

(defun parser-simple-rule ( matchf-list operator )
  `(lambda ()
     (apply ',operator ',matchf-list)) )

(defun parser-compound-rule ( matchf-list operator function )
  `(lambda ()
     (funcall ,function (apply ',operator ',matchf-list))) )

;; Compilers

(defun parser-compile-operator ( prod-right rule-builder &rest builder-args )
  "Compile a Match Function out of an anonymous rule which is
   fairly simple since we do not need to construct a match, only
   pass it through."

  (catch 'no-matches
    (make-anon-func "parser-operator"
      (apply rule-builder
        (parser-rule-right
          "parser-compile Warning! anonymous rule deleted with no matches in rule"
          prod-right)
        builder-args))
    ))

(defun parser-compile-production ( prod-right name rule-builder &rest builder-args )
  "bind an anonymous operator to a named Match Function"
  (catch 'no-matches

    (parser-match-function name
      (apply rule-builder
        (parser-rule-right
          (format
            "parser-compile Warning! rule %s deleted with no matches in rule"
            (symbol-name name))
          prod-right)
        builder-args))
   ))

;;----------------------------------------------------------------------
;; statement decomposition.
;;----------------------------------------------------------------------

(defun parser-token-statement ( syntax )
  "Compile a token into a Match Function."
  (parser-match-function (car syntax) (parser-interp-token syntax)))

;; statements are either production statements that produce
;; non-terminals or matching statements.

(defun parser-production-statement ( operator prod-right )
  "compile a statement with a name"
  ;; FIXME:
  ;; I changed this from lexical-let to let and it works instead of failing.
  ;; This means that I have a binding phase problem, and I need to figure out
  ;; where I need to use make-symbol.
  (let
    ((prod-symbol (car prod-right))
     (prod-rules  (cdr prod-right)))

    (unless (symbolp prod-symbol)
      (parser-diagnostic prod-symbol
        "Production Left interpreter"
        "left side of the production: an identifier"))

    (parser-compile-production prod-rules prod-symbol 'parser-compound-rule operator
      `(lambda ( production )
         (if production
           (parser-make-match
             (parser-match-consumed production)
             (parser-make-match-data '',prod-symbol (parser-match-data production))))))
    ))

(defun parser-operator-statement ( operator prod-right )
  (parser-compile-operator prod-right 'parser-simple-rule operator))

;; unfortunately I am forced to define the operator table for
;; parser-operator-production as well because parser-operator-production
;; "steals" the operator statement from parser-compile-definition
;; so it can be compiled as a production instead of a operator.

(defmacro parser-operator-map ( statement &rest operators )
  "compare STATEMENT against a OPERATORS list of symbol body
   pairs. Evaluate the body if STATEMENT eq symbol."
  `(cond
     ,@(mapcar
         (lambda ( op-map )
           `((eq ,statement ',(car op-map)) (,@(cadr op-map))) ) operators)
     ))

(defun parser-operator ( name )
  (parser-operator-map name
    (or 'parser-or)
    (+  'parser-positive-closure)))

(defun parser-operator-production ( name definition )
  "bypass normal anon compilation to bind it to a name."
  (let
    ((operator (parser-anon-op (car definition)))
     (prod-right (cdr definition)))

    (if (eq nil operator)
      (signal 'parser-syntactic-error
        (parser-diagnostic operator
          "Rule interpreter"
          "anonymous rule to bind to a name")))

    (parser-compile-production prod-right name 'parser-simple-rule operator)
    ))

;;----------------------------------------------------------------------
;; top level grammar statements.
;;----------------------------------------------------------------------

(defun parser-dispatch-unique ( table )
  "create the cond clauses for unique statements"
  (mapcar
    (lambda ( statement-map )
      `((eq keyword ',(car statement-map)) (funcall ,(cadr statement-map) syntax))) table))

(defun parser-dispatch-class ( implementation table )
  (list `((lexical-let
      ((lookup (cond
                 ,@(mapcar
                     (lambda (statement-map)
                       `((eq keyword ',(car statement-map)) ',(cadr statement-map))) table))))

      (if lookup
        (funcall ',implementation lookup syntax)))) ))

(defmacro parser-statement-dispatch ( grammar no-match &rest tables )
  `(let
     ((keyword (car ,grammar))
       (syntax  (cdr ,grammar)))

     (cond
       ,@(apply 'append (mapcar
             (lambda (table)
               (lexical-let
                 ((table-type (car table))
                  (table-def  (cdr table)))

                 (cond
                   ((eq table-type 'type)   (parser-dispatch-class (car table-def) (cdr table-def)))
                   ((eq table-type 'unique) (parser-dispatch-unique table-def)) )
                 )) tables ))
       (,no-match)
       )))

(defun parser-compile-definition ( term )
  "parser-compile-definition compiles grammar statements which are lists
   with a keyword as the first symbol."
  (unless (listp term)
    (signal 'parser-syntactic-error
      (parser-diagnostic term
        "parser definition"
        "expected a definition of token|or|and|define")))

  (parser-statement-dispatch term
    (signal 'parser-syntactic-error
      (parser-diagnostic keyword
        "parser definition"
        "definition keyword token|or|and|define"))

    (type parser-production-statement
      (and parser-and))

    (type parser-operator-statement
      (or parser-or)
      (+  parser-positive-closure))

    (unique
      (token   (lambda (syntax) (parser-token-statement syntax)))

      ;; we should only have two sexp in the name list, hence cadr
      (name    (lambda (syntax) (parser-operator-production (car syntax) (cadr syntax))))

      ;; define discards the match functions as a return value so
      ;; tokens and rules can be defined before they are used.
      (define  (lambda (syntax)
                 (mapcar 'parser-compile-definition syntax)
                 nil)) )
    ))

(defvar parser-mtable-init-size 13
  "initial size of the match-table objarray for storing match functions. the value
   was chosen based on the recommendation of prime numbers for good hashing.")

(defmacro parser-compile ( parser &rest definition )
  "compile a Recursive Descent parser from the given grammar."
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
                     ((parse (,(parser-compile-production definition 'start
                                 'parser-simple-rule 'parser-or)) ))
                     (if parse
                       ;; if we have a production return the position at which the
                       ;; parser stopped along with the AST.
                       (parser-make-match (parser-pos) (parser-match-data parse))
                       nil))
                   ))) ))
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

;;----------------------------------------------------------------------
;; utilities
;;----------------------------------------------------------------------

(defun parser-token-string ( start end )
  "Return a string of the input bounded by the token match."
  (filter-buffer-substring start end nil t))

(defun parser-interactive (parser)
  "run test-parser interactively for testing and debugging."
  (interactive "SParser? ")
  (lexical-let
    ((parse-result (funcall parser (point))))

    (message "PROD match? %s"
      (if parse-result
        (format "Yes matched to: %s, AST: %s" (car parse-result) (pp (cdr parse-result))
        "No")
      )) ))

(defmacro parser-trace-list ( list &rest productions )
  `(setq ,list
    '(
       ,@(mapcar
           (lambda (trace)
             `(,(symbol-name (car trace)) . ,(cadr trace))) productions)
       )))

(defun parser-debug (parser trace-list)
  "run test-parser interactively for testing and debugging."
  (interactive "SParser? 
STrace List? ")
  (let
    ((parser-trace-buffer (generate-new-buffer (format "parser-trace:%s" (symbol-name parser))))
      (parser-trace (eval trace-list)))

    (lexical-let
      ((parse-result (funcall parser (point))))

      (parser-trace-message
        (format "\nPROD match? %s\n"
          (if parse-result
            (format "Yes matched to: %s, AST: %s" (car parse-result) (pp-to-string (cdr parse-result))
              "No")
            ))) )

    ;; now that debugging has a buffer it's possible to make a mode, do slick things
    ;; like highlight the bounds of the match when the cursor is on the line of
    ;; a trace in the buffer.
    (pop-to-buffer parser-trace-buffer)
    ))

(provide 'parser)
