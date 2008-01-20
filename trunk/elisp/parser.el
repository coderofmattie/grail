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

;; -> Related Works

;; * CEDET.

;; CEDET appears to be a parser generator implemented on-top of a CLOS
;; emulation. This tool-set has developed towards the problem of
;; analyzing source code as a project which is bloated for simpler
;; requirements.

;; CEDET is likely to offer much higher performance than an un-optimized
;; recursive descent parser. If backtracking needs to be optimized
;; something like CEDET is a better choice than parser-compile.

;; * lalr for chicken Scheme

;; http://www.call-with-current-continuation.org/eggs/lalr.html

;; lalr implements a parser-compiler as a macro so it is similar in
;; spirit despite being a different class of parser.

;; tokenizing is a seperate phase as well.

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

;; 3. Recursion must be limited to the grammar, not the input. For example
;;    positive closure must be iterative, since it is bounded only by
;;    the input stream. This requirement makes it possible to approximate
;;    a recursion limit, important as Emacs does not have TCO.

;; ->Characteristics

;; parser-compile produces a no frills LL Recursive Descent parser.

;; The parsing characteristics are very similar to PEG grammars due to
;; the use of or and and as the primitive combination operators. All
;; operators are greedy.

;; left recursion is illegal, but I might want to add it
;; as a convenience.

;; ->Terminology

;; My reference for parsing terminology is the Dragon Book:

;; Compilers
;; Principles,Techniques,and Tools
;; Alfred V.Aho, Ravi Sethi, Jeffrey D.Ullman
;; 1986, Addison Wesley

;; ->TODO

;; 1. All of the PEG predicates
;; 2. re-document after all the code churn.
;; 3. Canonical tree walk implemented as parser-ast-node.

;; -> Phase 2

;; left recursion is currently forbidden because the lookup in the
;; production table will fail with a signal. to implement left
;; recursion binding needs to be delayed.

;; 1. re-write left recursion. This is essential to make grammars
;;    easy to write while avoiding infinite recursion problems.

;; my dynamic approach is looking essentially the same as "Packrat parsers
;; can be Left Recursive". It is obvious :)

;;    This will likely be a non-trivial hack requiring some extensive
;;    modifications to the parser. It would be nice if the surgery
;;    could be largely contained to parser-match-function.

;; this might be possible to solve dynamically. Need to check the
;; indirect recursion case, but the idea is to convert a stateless
;; recursion which goes infinite into a nesting, via a stack. It would
;; track the current production, and be cleared when a non-terminal
;; matches. If no non-terminal matches and it's already on stack then
;; skip the non-terminal.

;; 2. Implement memoization. whenever a backtrack clears the stack instead
;;    of discarding the stack it should save it instead.

(require 'cl)
(require 'mattie-elisp) ;; define-error, make-anon-func, list-filter-nil

(define-error parser-compile-error   "parser error")
(define-error parser-syntactic-error "syntactic error" parser-compile-error)
(define-error parser-semantic-error  "semantic error" parser-compile-error)

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
  "Restore the previous parser position by discarding the top of the parser-position stack.
   Always returns nil so it can be used as a failure Match Result."
  (pop parser-position)
  (goto-char (parser-pos))
  nil)

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

(defun parser-make-production-match ( data )
  "Create a Match Result cons of (t . match data)."
  (cons t data))

(defun parser-make-token-match ( data )
  "Create a Match Result cons of (t . match data)."
  (cons (parser-consumed) data))

(defun parser-match-p ( match-result )
  "Return the input consumed by the match"
  (and (consp match-result) (car match-result)))

(defsubst parser-match-data ( match-result )
  "Return the match data."
  (cdr match-result))

(defun parser-consume-token ( match-result )
  "consume a token converting it to a production match if not so already."
  (if (numberp (car match-result))
    (progn
      (parser-advance (car match-result))
      (parser-make-production-match (cdr match-result)))
    match-result) )

(defun parser-combine-match-data ( newer older )
  (cond
    ((eq nil older) newer)
    ((and (listp newer) (listp older)) (append older newer))
    ((cons newer older)) ))

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
;; Primitive Operators
;;----------------------------------------------------------------------

;; The parser uses two primitive operators: parser-and, parser-or as
;; primitives to construct matching behavior. Significantly parser-and
;; can backtrack and parser-or never does.

;; and/or have the same essential meaning as the lisp and/or forms
;; with two specializations. Both functions treat their argument lists
;; as a list of Match Functions.

;; These are purely matching functions: Match Results as opaque.

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
            (throw 'match (parser-consume-token production)))
          ))
      )))

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

                           (unless production
                             (throw 'backtrack nil))

                           (parser-match-data (parser-consume-token production)) )))
                     match-list)) ))
    (if production
      (progn
        (parser-pop)
        (parser-make-production-match production)) ;; nil AST could be filtered here.
      (parser-backtrack)) ))

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

      (if (eq definition nil)
        (signal 'parser-semantic-error (format "production %s not defined, likely left recursion." id))
        (progn
          (fset (intern id match-table) (eval definition))
          (intern id match-table))
        ))))

;;----------------------------------------------------------------------
;; tokens
;;----------------------------------------------------------------------

(defun parser-token-bounds ( type capture )
  "Return the bounds of the capture from match-{beg,end} with the
   upper bound adjusted by decrement to inclusive. The type
   returned is chosen with a quoted type constructor symbol like
   cons or list."
  (eval `(,type (match-beginning capture) (match-end capture)) ))

;; The token part of the grammar definition contains a great deal of flexibility
;; or construction options for tokens.

;; parser-token-function builds the matching part while
;; parser-token-constructor focuses on constructing Match Result data
;; for the token.

(defun parser-token-constructor ( constructor )
  "Construct the Match Data constructor for the token as a single s-exp."

  (cond
    ((eq nil constructor)    `(parser-token-bounds 'cons 0))
    ((listp constructor)     `(apply ',(make-anon-func "parser-user-handler") (parser-token-bounds 'list 0)))
    ((functionp constructor) `(apply ',constructor (parser-token-bounds 'list 0)))
    ((numberp constructor)   `(parser-token-bounds 'cons ,constructor))
    ((symbolp constructor)   `(quote ',constructor))

    ;; all other constructor types are un-handled.
    ((signal 'parser-syntactic-error
       (parser-diagnostic constructor
         "parser-token-constructor"
         "lambda|function|number|symbol")))) )

(defun parser-token-function ( syntax )
  "Generate a token Match Function lambda."
  `(lambda ()
     (if (looking-at ,(car syntax))
       (parser-make-token-match ,(parser-token-constructor (cadr syntax)))
       )) )

;;----------------------------------------------------------------------
;; Parser Generator
;;----------------------------------------------------------------------

;; The parser is built up from primitives with two combination generators
;; that nest Match Functions.

(defun parser-predicate-function ( match-func predicate )
  "Pass the Match Result of match-func to predicate."
  ;; any quoting issues should be implemented here.

  `(lambda ()
     (funcall ,predicate
       ,(if (listp match-func)
          `(funcall ,match-func)
          `(funcall ',match-func))
       )) )

(defun parser-compound-function ( match-func function )
  "Pass the Match Function match-func to predicate."
  `(lambda ()
     (funcall ,function ',match-func)) )

;;----------------------------------------------------------------------
;; Closures
;;----------------------------------------------------------------------

;; these two predicate operators are the two primitives needed to
;; implement greedy matching.

;; FIXME: trace point in the closures ?

(defun parser-positive-closure ( match-func )
  "A positive closure compound function of unbounded greed."
  (lexical-let
    ((closure nil))

    (do ((production (funcall match-func) (funcall match-func)))
        ((eq nil production) (if (not (eq nil closure))
                               (parser-make-production-match closure)))
      (lexical-let
        ((data (parser-match-data (parser-consume-token production) )))
        (setq closure (parser-combine-match-data data closure) ))
      )))

(defun parser-optional-closure ( match-result )
  "An optional closure predicate function that converts match
   failures to a match succeeded with nil match data."
  (if match-result
    (parser-consume-token match-result)
    (parser-make-production-match nil)) )

;;----------------------------------------------------------------------
;; Predicate Generators
;;----------------------------------------------------------------------

(defun parser-positive-function ( match-func )
  "Generate a positive closure of match-func."
  (parser-compound-function match-func ''parser-positive-closure))

(defun parser-optional-function ( match-func )
  "Generate an optional closure of match-func."
  (parser-predicate-function match-func ''parser-optional-closure))

(defun parser-kleene-function ( match-func )
  "Generate a kleene closure of match-func."
  (parser-predicate-function
    (parser-positive-function match-func) ''parser-optional-closure))

(defun parser-production-function ( name match-function )
  "Generate a predicate that cons's a production identifier to
   the Match Result data."
  (parser-predicate-function match-function
    `(lambda ( production )
       (if production
         (progn
           (setcdr production
             (if (numberp (car production))
               (cons '',name (cdr production))
               (list '',name (cdr production)) ))
           production)) )))

;;----------------------------------------------------------------------
;; production right side evaluation
;;----------------------------------------------------------------------

(defmacro parser-operator-map ( statement &rest operators )
  "compare STATEMENT against a OPERATORS list of symbol body
   pairs. Evaluate the body if STATEMENT eq symbol."
  `(cond
     ,@(mapcar
         (lambda ( op-map )
           `((eq ,statement ',(car op-map)) (,@(cadr op-map))) ) operators)
     ))

;; Production right side evaluation is introduced before parser compilation
;; so that the recursion of evaluating a grammar statement can be inserted
;; into the parser compilation functions.

;; This simplifies the top level statement compilation by rendering the
;; recursive part of the statement an opaque object fed to the
;; parser compiler.

(defun parser-eval-rule-right ( rule )
  "Evaluate a symbol or statement in the production right resulting in a bound Match Function."

  (cond
    ((listp rule) (parser-compile-definition rule))
    ((symbolp rule) (parser-match-function rule))

    (signal 'parser-syntactic-error
      (parser-daignostic rule
        "Rule Right interpreter"
        "expected a grammar list e.g: token,and,or ; or a symbol as a rule or token reference"))
    ))

(defun parser-rule-right ( nil-warning rule-list )
  "Apply parser-eval-rule-right to a production right list."
  (lexical-let
    ((matchf-list (list-filter-nil (mapcar 'parser-eval-rule-right rule-list))))

    (if matchf-list
      matchf-list
      (progn
        (message "%s" nil-warning)
        (throw 'no-matches nil)))
    ))

(defun parser-node-function ( prod-operator prod-right )
  "Bind a set of Match functions to a primitive operator, a single match is optimized
   as itself."
  (let
    ((matchf-list (parser-rule-right
                    "parser-compile Warning! rule deleted with no matches in rule"
                    prod-right)))

    (if (eq nil (cdr matchf-list))
      (car matchf-list)
      `(lambda ()
         (apply ',prod-operator ',matchf-list )) )))

;;----------------------------------------------------------------------
;; Compilation
;;----------------------------------------------------------------------

(defun parser-compile-to-symbol ( function &optional name )
  "compile a Match Function as either a un-interned symbol when name is nil or
   a symbol queried by parser-match-function when name is given"
  (message "compiling %s" (pp-to-string function))
  (if name
    (parser-match-function name function)
    (make-anon-func "parser-operator" function)))

(defun parser-compile-production ( name match-function )
  "Compile a named production given a name and a generated Match Function."
  (parser-compile-to-symbol (parser-production-function name match-function) name))

;;----------------------------------------------------------------------
;; Grammar Interpreter
;;----------------------------------------------------------------------

(defun parser-lookup-table ( table body )
  "Expand into a lookup table contained within a cond clause that executes body if
   a match is found in the table."
  `((lexical-let
      ((lookup (cond
                 ,@(mapcar
                     (lambda (statement-map)
                       `((eq keyword ',(car statement-map)) ',(cadr statement-map))) table))))
      (if lookup
        ,body))) )

;; These two functions return clauses within lists so that all the clauses generated
;; can be merged with apply into a single list suitable for inserting into a cond
;; form.

(defun parser-dispatch-operator ( table )
  "Compile operator statements with a predicate function generator from the table
   and the parser-and operator."
  (list (parser-lookup-table table
          `(parser-compile-to-symbol
             (funcall lookup (parser-node-function 'parser-and syntax))) )))

(defun parser-dispatch-production ( table )
  "Compile productions using the identifier from the statement and the Match Function
   generated by the lambda in the table."
  (list (parser-lookup-table table
          `(parser-compile-production (car syntax) (funcall lookup (cdr syntax))) )))

(defun parser-dispatch-statement ( table )
  "create the cond clauses for unique statements"
  (mapcar
    (lambda ( statement-map )
      `((eq keyword ',(car statement-map)) (funcall ,(cadr statement-map) syntax))) table))

(defmacro parser-grammar ( grammar expected &rest tables )
  "construct a more readable syntax for the grammar statement interpreter using nested cond
   forms as the mechanism. The compilation of Match Functions is implemented in the macro
   allowing the form to focus on parser generation."
  `(let
     ((keyword (car ,grammar))
       (syntax  (cdr ,grammar)))

     (cond
       ,@(apply 'append
           (mapcar
             (lambda (gr-table)
               (lexical-let
                 ((table-type (car gr-table))
                  (table-def  (cdr gr-table)))

                 (cond
                   ((eq table-type 'operators)   (parser-dispatch-operator  table-def))
                   ((eq table-type 'productions) (parser-dispatch-production table-def))
                   ((eq table-type 'statements)  (parser-dispatch-statement table-def))
                   ))) tables ))

           ((signal 'parser-syntactic-error
              (parser-diagnostic keyword
                "parser definition"
                ,expected)))
       )))

(defun parser-compile-definition ( statement )
  "parser-compile-definition compiles grammar statements which are lists
   with a keyword as the first symbol."

  (unless (listp statement)
    (signal 'parser-syntactic-error
      (parser-diagnostic term
        "parser definition"
        "expected a definition list such as token|or|and|define")))

  (parser-grammar statement
    "statement token|or|and|define or operator"

    (operators
      (+  parser-positive-function)
      (?  parser-optional-function)
      (*  parser-kleene-function))

    (productions
      (and     (lambda (syntax)
                 (parser-node-function 'parser-and syntax)))

      (token   (lambda (syntax)
                 (parser-token-function syntax))) )

    (statements
      (or      (lambda (syntax)
                 (parser-compile-to-symbol (parser-node-function 'parser-or syntax))))

      ;; we should only have two sexp in the name list, hence cadr
      (name    (lambda (syntax)
                 (parser-remember-production (car syntax))
                 (parser-compile-definition (cadr syntax))))

      ;; define discards the match functions as a return value so
      ;; tokens and rules can be defined before they are used.
      (define  (lambda (syntax)
                 (mapcar 'parser-compile-definition syntax)
                 nil)) )
    ))

;;----------------------------------------------------------------------
;; macro interface
;;----------------------------------------------------------------------

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
                     ((parse (,(parser-compile-production 'start
                                 (parser-node-function 'parser-or definition))) ))
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
