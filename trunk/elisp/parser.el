;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;; Copyright (C) 2008 Mike Mattie
;; Description: Recursive Descent parser compiler with a lisp macro
;;              interface.
;;----------------------------------------------------------------------

;; ->Features

;; 0. Parsing is integrated directly into the language instead of
;;    being a separate tool.

;;    Evaluate a parser-compiler form with a symbol and a grammar
;;    to generate a parser bound to your symbol.

;; 1. PEG like grammar.

;; 2. Parse Data is is not hard-wired. Tokens can perform user defined
;;    functions or construct data from the bounds of a token match in
;;    the input.

;;    The AST data structure is a tree generated by the parser.
;;    non-terminal productions add nodes, tokens add leaves.

;; 3. Grammar recursive but not input recursive, important because
;;    Emacs does not have TCO.

;; ->TODO

;;   -> Phase 1: correctness

;; 0. tracing start production does not work.
;; 1. All of the PEG predicates (missing not)

;; 2. re-document after all the code churn.
;;    need to C-h f functions in various context to see how the DocStrings look +
;;    read the DocString Style Guide.

;; 3. Canonical tree walk implemented as parser-ast-node.
;; 4. commented out message string in compile needs to be a debugging
;;    option
;; 5. Check the generated code for anything silly, keeping in mind that
;;    it is generated code.

;;    -> Phase 2: optimization

;; 2. Implement packrat backtrack optimization.

;;    better idea. When a backtrack occurs a nil will be returned to
;;    the parse-ast-descend predicate. Normally the parse tree constructed
;;    so far would be discarded. Instead filter the terminals out, and
;;    keep them in a table.

;; ->Terminology

;; My reference for parsing terminology is the Dragon Book:

;; Compilers
;; Principles,Techniques,and Tools
;; Alfred V.Aho, Ravi Sethi, Jeffrey D.Ullman
;; 1986, Addison Wesley

(require 'cl)
(require 'mattie-elisp) ;; USES define-error make-anon-func list-filter-nil

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

(defun parser-pos ()
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
;; Parser Tracing
;;----------------------------------------------------------------------

;; A tracing facility that can be selectively turned on and off for
;; productions. When tracing is turned on the result of all matches
;; attempted are printed to a buffer, or the Message buffer.

;; A report of how the compiled parser matched the input stream is
;; vital to developing a working grammar.

(defun parser-trace-message ( format &rest args )
  "Prints to a trace buffer instead of the Message buffer."
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
      ((toggle (apply 'or (mapcar (lambda ( trace-on )
                                    ;; eq comparison of symbols does not work. A string
                                    ;; comparison is used for matching.

                                    ;; FIXME: symbol-value may remove this wart.
                                    (if (equal (symbol-name production) (car trace-on))
                                      (cdr trace-on)))
                            parser-trace) )))
      ;; a cons cell is returned so that a false value for the trace flag can be returned
      ;; without negating the truth value of the predicate itself.
      (if toggle
        (cons t toggle)
        (cons nil nil) )) ))

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
;; Match Result
;;----------------------------------------------------------------------

;; The parser functions have a standard structure for returning the
;; result of a Match Function.

;; nil | (parser . AST ).

;; nil indicates failure to match.

;; A successful match has a parser and AST parts.

;; The parser part is either a count of input characters consumed
;; for a terminal or t|nil for non-terminals. When the parser
;; position is advanced the count is replaced with t.

;; The AST part is created by a token action handler or a combination
;; operator like and.

;; terminal     ( match-symbol . ( start . end ) | "user value")
;; non-terminal ( match-symbol . "a list/tree of AST" )

(defun parser-make-production-match ( tree )
  "Make a matched non-terminal Match Result."
  (cons t tree))

(defun parser-make-token-match ( data )
  "Make a matched terminal Match Result."
  (cons (parser-consumed) data))

(defun parser-match-p ( match-result )
  (unless (consp match-result)
    (throw 'parser-match-fail nil))

  "Return the parse part of a Match Result."
  (car match-result))

(defsubst parser-match-data ( match-result )
  "Return the data of a Match Result."
  (cdr match-result))

;;----------------------------------------------------------------------
;; AST tree constructors
;;----------------------------------------------------------------------

;; The AST constructor builds a tree out of the Match Results created
;; by parsing.

;; Each non-terminal created by the and statement creates a new list
;; that is dynamically scoped as parse-tree. The beginning of the list
;; is stored lexically.

;; As the parse progresses terminals are added with
;; parse-ast-append-match. When a production is derived for a
;; non-terminal the scope of parse-tree is exited, and the completed
;; sub-tree is joined to the parent production's parse-tree which is
;; no longer shadowed.

;; If there is no parent parse-tree bound it can only be the start
;; symbol, so the value of head is returned from the function as the
;; completed AST tree.

(defun parser-ast-append ( production )
  "append to the AST."
  (lexical-let
    ((new-tail (cons production nil)))

    (setcdr parse-tree new-tail)
    (setq parse-tree new-tail) ))

(defun parser-?consume-match ( match-result )
  "consume a token converting it to a production match if not so already."

  (catch 'consumed-match
    (lexical-let
      ((match-status (parser-match-p match-result))
       (match-data   (parser-match-data match-result)))

      (unless match-status
        (throw 'parser-match-fail nil))

      (if (numberp match-status)
        (progn
          (parser-advance match-status)
          (parser-ast-append match-data)
          (throw 'consumed-match (parser-make-production-match nil)))

        ;; alternative to a token is a possible un-consumed production.
        (if match-data
          (progn
            (parser-ast-append match-data)
            (throw 'consumed-match (parser-make-produciton-match nil)))) ))

      match-result))

(defun parser-ast-descend ( non-terminal func-match )
  "start a new AST level"
  (catch 'parser-match-fail
    (lexical-let
      ((unattached-node (cons non-terminal nil)))

      ;; this step populates the detached node
      (let
        ((parse-tree unattached-node))
        (parser-?consume-match (funcall func-match)) )

      ;; Attach the tree or return it as the completed AST.
      (if (boundp 'parse-tree)
        (progn
          (parser-ast-append unattached-node)
          (parser-make-production-match nil))
        (parser-make-production-match unattached-node)) )))

;;----------------------------------------------------------------------
;; Primitive Operators
;;----------------------------------------------------------------------

;; The parser uses two primitive operators: parser-and, parser-or as
;; primitives to construct matching behavior. Significantly parser-and
;; both backtracks and populates nodes - whereas or treats matches as
;; opaque.

;; and/or have the same logical behavior in regards to the boolean
;; result of evaluating Match Functions as the lisp and/or forms.

(defun parser-or ( &rest match-list )
  "Combine Match Functions by or ; the first successful match is returned.
   nil is returned if no matches are found"
  (catch 'match
    (dolist (match-func match-list)
      (parser-trace-on match-func

        (catch 'parser-match-fail

          ;; if there was not a non-local exit abort the traverse of match-list
          ;; with a successful match.
          (throw 'match
            (lexical-let
              ((match-result (funcall match-func)))

              (parser-trace-match match-func match-result)
              (parser-?consume-match match-result)) )) ))))

(defun parser-and ( &rest match-list )
  "combine the matches with and. all of the match objects must return non-nil
   in the parser part or the parser will backtrack and return nil."
  (parser-push)

  (if (catch 'parser-match-fail
        (dolist (match-func match-list t)
          (parser-trace-on match-func

            (lexical-let
              ((match-result (funcall match-func)))
              (parser-trace-match match-func match-result)
              (parser-?consume-match match-result) )) ))
    (progn
      (parser-pop)
      (parser-make-production-match nil))
    (parser-backtrack) ))

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
          (intern id match-table)) ))))

;;----------------------------------------------------------------------
;; tokens
;;----------------------------------------------------------------------

;; The token part of the grammar definition contains a great deal of flexibility
;; or construction options for tokens.

;; parser-token-function builds the matching part while
;; parser-token-constructor focuses on constructing Match Result data
;; for the token.

(defun parser-token-bounds ( type capture )
  "Return the bounds of the capture from match-{beg,end} with the
   upper bound adjusted by decrement to inclusive. The type
   returned is chosen with a quoted type constructor symbol like
   cons or list."
  (funcall type (match-beginning capture) (match-end capture)) )

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
     (if (looking-at ,(cadr syntax))
       (parser-make-token-match (cons '',(car syntax) ,(parser-token-constructor (caddr syntax)))) )))

;;----------------------------------------------------------------------
;; Parser Generator
;;----------------------------------------------------------------------

;; The parser is built up from primitives with two combination generators
;; that nest Match Functions.

(defun parser-predicate-function ( match-func predicate )
  "Pass the Match Result of match-func to predicate."
  `(lambda ()
     (funcall ,predicate
       ,(cond
          ((and (listp match-func)
                (functionp match-func)) `(funcall ,match-func))
          ((functionp match-func)       `(funcall ',match-func))

          ((symbol-value 'match-func)) ))) )

(defun parser-compound-function ( match-func function )
  "Pass the Match Function match-func to predicate."
  `(lambda ()
     (funcall ,function ',match-func)) )

;;----------------------------------------------------------------------
;; Closures
;;----------------------------------------------------------------------

;; these two predicate operators are the two primitives needed to
;; implement greedy matching.

;; TODO: if matched-once was changed to match-count and incremented
;;       with incf, it would not be terribly hard to implement something
;;       like ?4 which would minimally match 4 times.
(defun parser-positive-closure ( match-func )
  "A positive closure compound function of unbounded greed."
  (lexical-let
    ((matched-once nil))

    (do ((production (funcall match-func) (funcall match-func)))
        ((eq nil production) (if (not (null matched-once))
                               (parser-make-production-match nil)))

      (setq matched-once t)
      (parser-?consume-match production)) ))

(defun parser-optional-closure ( match-result )
  "An optional closure predicate function that converts match
   failures to a match succeeded with nil match data."

  (catch 'parser-match-fail
    (parser-?consume-match match-result))

  (parser-make-production-match nil))

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
  "Pass the Match Function to parser-ast-descend to create a sub-tree
   in the AST, scoping parse-tree. This is used by non-terminals
   only."
  `(lambda ()
     (parser-ast-descend '',name ',match-function)))

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
  ;; (message "compiling %s" (pp-to-string function))
  (if name
    (parser-match-function name function)
    (make-anon-func "parser-operator" function)))

(defun parser-compile-production ( name match-function )
  "Compile a named production given a name and a generated Match Function."
  (parser-compile-to-symbol (parser-production-function name match-function) name))

;;----------------------------------------------------------------------
;; Interpreter Syntax
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

(defun parser-dispatch-operator ( table )
  "Compile operator statements with a predicate function generator from the table
   and the parser-and operator."
  (list (parser-lookup-table table
          `(parser-compile-to-symbol
             (funcall lookup (parser-node-function 'parser-and syntax))) )))

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
                   ((eq table-type 'statements)  (parser-dispatch-statement table-def))
                   ))) tables ))

           ((signal 'parser-syntactic-error
              (parser-diagnostic keyword
                "parser definition"
                ,expected))) )))

;;----------------------------------------------------------------------
;; Grammar Interpreter.
;;----------------------------------------------------------------------

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

    (statements
      (token   (lambda (syntax)
                 (parser-compile-to-symbol (parser-token-function syntax) (car syntax))))

      (and     (lambda (syntax)
                 (parser-compile-production (car syntax)
                   (parser-node-function 'parser-and (cdr syntax)))))

      (or      (lambda (syntax)
                 (parser-compile-to-symbol (parser-node-function 'parser-or syntax))))

      (name    (lambda (syntax)
                 (lexical-let
                   ;; first try to compile
                   ((compiled (parser-compile-definition (cadr syntax))))

                   (message "compiled is %s" (pp-to-string compiled))
                   ;; we should get an anon compiled function, otherwise using
                   ;; name is nutty.
                   (if (string-equal "parser-operator" compiled)
                     (parser-compile-to-symbol (symbol-function compiled) (car syntax))
                     (signal parser-syntax-error "name statement is for naming operators/or only"))
                   )))

      ;; define discards the match functions as a return value so
      ;; tokens and rules can be defined before they are used.
      (define  (lambda (syntax)
                 (mapcar 'parser-compile-definition syntax)
                 nil)) )))

;;----------------------------------------------------------------------
;; macro interface
;;----------------------------------------------------------------------

(defvar parser-mtable-init-size 13
  "initial size of the match-table objarray for storing match functions. the value
   was chosen based on the recommendation of prime numbers for good hashing.")

(defmacro parser-compile ( parser &rest definition )
  "parser-compile PARSER &rest GRAMMAR

  parser-compile Implements a Recursive Descent parser compiler
  with a Lisp Macro interface. The parser-compiler is a symbol
  and a grammar definition in sexp's. Evaluation of the form
  generates a complete scanner-less parser bound to the PARSER
  symbol given as the first argument.

  The generated parser is bound fset so the symbol is used as a
  function: funcall is not necessary. Parsers do not conflict
  with each other unless you tried to nest them.

  The sole argument to the parser is a starting position in the
  current buffer. The return value is the resume position for
  another parse along with the AST generated, or nil. A single
  production of the start symbol is returned per-call.

  GRAMMAR is essentially PEG in sexp form with some additional
  features for tokens. There is a implicit start symbol 'start
  for the toplevel of the grammar definition. The start symbol's
  operator is or.

  The grammar is recursively interpreted. Left recursion of any
  sort is forbidden because a Left Production is not stored in
  the Production Table until the Production right is completely
  evaluated. I consider this a mis-feature and would like to
  implement delayed binding.
  "

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
                       (cons (parser-pos) (parser-match-data parse))
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
