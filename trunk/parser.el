;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;; Copyright: Mike Mattie (2008)
;;----------------------------------------------------------------------

;; ->Summary.

;; parser.el aims to be a lightweight implementation of Recursive Descent
;; parsing.

;; -> Application.

;; Building or extracting a nested data structure by parsing static
;; text.

;; -> Comparison with Emacs parsing facilities.

;; Emacs supports parsing in two forms: Regular Expressions and tools
;; like Syntax Tables geared towards using the parse results to
;; annotate buffers with text-properties and overlays. This works well
;; for overlaying functions that interpret the meaning of text in the
;; buffer, such as syntax highlighting, on a paradigm of unstructured
;; text.

;; When you need to build an interface that rests entirely on the
;; parse analysis to the degree that the user or program does not
;; modify or traverse the buffer at a character level this tool
;; simplifies constructing a nested data structure that maps the
;; parsing analysis onto the text of the buffer with beginning and end
;; positions.

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
;; implementation is now more complex with match functions replacing
;; the cond clause form; allowing previous definitions of a token or
;; productions can be referenced later in the definition. Backtracking
;; was another essential addition once the and non-terminal was added.

;; ->Requirements

;; 1. Easy to use for trivial problems, powerful enough to solve
;;    most of them.

;; 2. Data structures are orthogonal to the parser and not hard-wired
;;    in. Leafs and nodes of the AST are constructed with beginning
;;    and ending positions of the text in the buffer.
;;
;;    This allows the user to choose between positions, markers, overlays
;;    according to the requirements.

;; ->TODO

;; 1. define list where Matches can be defined without inserting a
;;    match at the definition point

;; 2. optional matching with ? for Match Function references.

;; 3. Canonical tree walk implemented as parser-ast-node.

;;----------------------------------------------------------------------
;; Backtracking.
;;----------------------------------------------------------------------

;; parser-position

;; The position of the parser in the input stream is maintained as a
;; stack of indexes into the buffer. As matching progresses the top of
;; the stack (car) is updated. The push and pop operations copy a
;; position to a next or previous stack position. backtrack discards
;; the top of the stack.

(defun parser-pos () ;; tested
  "Return the current position of the parser in the buffer"
  (car parser-position))

(defun parser-push ()
  "Copy the parser position to a new stack level so the parser can backtrack when necessary."
  (setq parser-position (cons (car parser-position) parser-position)))

(defun parser-pop ()
  "Copy the parser position to a previous stack level When the possibility of a backtrack
   has been eliminated by matching."
  (let
    ((current (parser-pos)))
    (setq parser-position (cons (car parser-position) (cddr parser-position)))
    ))

(defun parser-backtrack ()
  "Restore the previous parser position by discarding the top of the parser-position stack."
  (setq parser-position (cdr parser-position))
  (goto-char (parser-pos)))

(defun parser-advance ( distance )
  "Add distance to the parsing position on the current stack level.
   The advancing of the parser is done relative to the current
   position so that a successful match can advance 0 characters.
   This is important for optional matches such as the \"?\" meta-symbol.
   They want to indicate a successful match without moving the parser position."

  ;; we are going to get zero's as a unfortunate side-effect of
  ;; keeping the nesting of combination operators simple. Avoid NOP
  ;; work with a quick test.

  (if (> distance 0)
    (progn
      (setq parser-position (cons (+ distance (car parser-position)) (cdr parser-position)))
      (goto-char (parser-pos)))
    ))

(defun parser-next ()
  "Compute the next parser position from the length of the entire regex's current match, plus one."
  (+ 1 (- (match-end 0) (match-beginning 0))))

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

;;----------------------------------------------------------------------
;; Combination Operators
;;----------------------------------------------------------------------

;; the parser uses two combination operators: parser-and, parser-or as
;; nodes in the parser tree. Significantly parser-and can backtrack
;; and parser-or never does.

;; and/or have the same essential meaning as the lisp and/or forms
;; with two specializations. Both functions treat their argument lists
;; as a list of match-functions. Also the parser-and function returns
;; a list of all the AST parts of the Match Results, instead of
;; returning the last value.

(defun parser-or ( &rest match-list )
  "Combine match objects by or where the first successful match is returned.
   nil is returned if no matches are found"
  (catch 'match
    (dolist (match match-list)
      (lexical-let
        ((match-result (funcall match)))
        (if match-result
          (progn
            (parser-advance (car match-result))
            (throw 'match (cons 0 (cdr match-result)))))
        ))
    (throw 'match nil) ;; this is what failure looks like :)
    ))

(defun parser-and ( &rest match-list )
  "combine the matches with and. all of the match objects must return non-nil
   in the parser part or the parser will backtrack and return nil."
  (parser-push)

  (lexical-let
    ((ast (catch 'backtrack
            ;; we want to gather all the matches, so mapcar across the match objects.
            (mapcar
              (lambda (match)
                (lexical-let
                  ((match-result (funcall match)))

                  (if match-result
                    (progn
                      ;; on match advance the parser and return the AST
                      (parser-advance (car match-result))
                      (cdr match-result))

                    (throw 'backtrack nil))
                  )) match-list)
            )))
   (if ast
     (progn
       (parser-pop)
       (cons 0 ast)) ;; would be nice to filter optional matches here.
     (progn
       (parser-backtrack)
       nil))
   ))

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

;; make-match and get-match implement a table of match functions.

;; Match functions are lambdas that take no arguments and return Match
;; Result values. They assume that the compiled parser has scoped a
;; parser-position stack to determine the parsing position in input.

;; match-table is objarray created at macro scope, and does not appear
;; in the compiled form.

(defun parser-make-match ( symbol function ) ;; tested
  "parser-make-match takes ( symbol function ) and returns a symbol
   stored in the parser's match-table with the evaluated lambda
   bound"
  (lexical-let
    ((new-name (symbol-name symbol)))

    (if (intern-soft new-name match-table)
      (throw 'semantic-error (format "illegal redefinition of match %s" new-name)))

    (fset (intern new-name match-table) (eval function))
    (intern new-name match-table)
    ))

(defun parser-get-match ( symbol ) ;; tested
  "return the compiled match for symbol, or throw a semantic-error if it does not
   exist"
  (lexical-let
    ((existing-name (symbol-name symbol)))

    (unless (intern-soft existing-name match-table)
      (throw 'semantic-error (format "unkown match %s" existing-name)))

    (intern existing-name match-table)))

;;----------------------------------------------------------------------
;; tokens
;;----------------------------------------------------------------------

(defun parser-build-token ( identifier ) ;; tested
  "parser-make-token is a built-in constructor that records the analysis
   and the location of the text (id . (begin . end))"
  (cons identifier (cons (match-beginning 0) (match-end 0))))

(defun parser-make-anon-func ( sexp ) ;; tested
  "bind an un-evaluated anonymous function to an un-interned symbol"
  (let
    ((anon-func (make-symbol "parser-user-handler")))
    (fset anon-func (eval sexp))
    anon-func))

;; the token interpreter was split into two functions to isolate the
;; flexibility of tokens (user functions or return values for
;; constructing AST) from the hard-wired parser part.

(defun parser-interp-token-action ( identifier constructor ) ;; tested
  "Translate the AST constructor part of a token definition into Elisp."

  (unless (symbolp identifier)
    (throw 'syntax-error (parser-diagnostic identifier
                           "parser token"
                           "identifier: An unbound symbol used as an identifier"
                           )))

  ;; Warning: this code is very touchy, double quotation of AST
  ;; symbols is required. the saving grace is that the symbols don't
  ;; have a variable value bound so it fails noisily when the
  ;; quotation is incorrect.
  (cond
    ((eq nil constructor)    `(parser-build-token (quote ',identifier)))
    ((listp constructor)     `(,(parser-make-anon-func constructor) (match-beginning 0) (match-end 0)))
    ((functionp constructor) `(,constructor (match-beginning 0) (match-end 0)))
    ((symbolp constructor)   `(quote ',constructor))

    ;; all other constructor types are un-handled.
    ((throw 'syntax-error (parser-diagnostic identifier
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
         (cons (parser-next) ,(parser-interp-token-action identifier constructor))
         nil
         ))
    ))

(defun parser-compile-token ( syntax ) ;; tested
  "Compile a token definition into a Match Function."
  (parser-make-match (car syntax) (parser-interp-token syntax)))

;;----------------------------------------------------------------------
;; productions
;;----------------------------------------------------------------------

(defun parser-interp-production ( production )
  "Translate the definition of a production, or a reference to a production
   into a match object symbol."

  (cond
    ((listp production) (parser-compile-definition production))
    ((symbolp production) (parser-get-match production))

    (throw 'syntax-error
      (parser-daignostic production
        "interpret definition"
        "expected a definition as a list, or a symbol as a production/token reference"))
    ))

;; after definitions are interpreted they are evaluated or compiled and
;; stored as match functions.

(defun parser-curry-production ( identifier combine-operator &rest grammar )
  "Compile a match object with a combine operator and a match function list.
   I think curry is applicable, but largely it was named curry so I could
   create the parser-compile-production macro."
  (unless (symbolp identifier)
    (parser-diagnostic identifier
      "compile production"
      "match identifier"))

    (parser-make-match identifier
      `(lambda ()
         (lexical-let
           ((result (apply ',combine-operator ',grammar)))
         (if result
           (cons (car result) (cons ',identifier (cdr result)))
           nil)
           ))
      ))

;; filter function in devel @ broken.el
(defmacro parser-compile-production ( combine-function production-list )
  "parser-compile-production simplifies the syntax of interpreting and compiling
   a production. The construct looks hairy because it combines two operations
   with quoting necessitated by apply. This macro mechanizes the tricky part
   to enhance code readability."
  `(apply 'parser-curry-production      ;; make a match function
     (car ,production-list)             ;; the identifier of the production
     ',combine-function                 ;; the combine operator
     ;; need to filter nil from define lists here
     (mapcar 'parser-interp-production (cdr ,production-list)))) ;; interpret the matching definition.

(defun parser-compile-definition ( term )
  "parser-compile-definition is the recursive heart of the compiler."
  (unless (listp term)
    (throw 'syntax-error (parser-diagnostic term
                           "parser definition"
                           "expected a definition of token|or|and")))

  (lexical-let
    ((keyword (car term))
      (syntax (cdr term)))

    (if (listp keyword)
      (parser-compile-definition keyword)

      (cond
        ((eq keyword 'token) (parser-compile-token syntax))
        ((eq keyword 'or)    (parser-compile-production parser-or syntax))
        ((eq keyword 'and)   (parser-compile-production parser-and syntax))
;;         ((eq keyword 'define) (mapcar 'parser-interp-production syntax) nil)
        ((throw 'syntax-error (parser-diagnostic term
                                "parser definition"
                                "definition keyword token|or|and")))
        ))
    ))

;;----------------------------------------------------------------------
;; parser-compile macro
;;----------------------------------------------------------------------

(defvar parser-mtable-init-size 13
  "initial size of the match-table objarray for storing match functions. the value
   was chosen based on the recommendation of prime numbers for good hashing.")

(defmacro parser-compile ( parser &rest definition )
  "compile a LL parser from the given grammar."
  (let
    ;; create a symbol table to store compiled terminal and
    ;; non-terminal match functions
    ((match-table (make-vector parser-mtable-init-size 0)))

    (lexical-let
      ((compiled (catch 'syntax-error
                   `(lambda ( start-pos )
                      (let
                        ((parser-position (cons start-pos nil))) ;; initialize the backtrack stack
                        (save-excursion
                          (goto-char start-pos)
                          ;; note that the start symbol of the grammar is built in as an or combination
                          ;; of the top-level definitions.
                          (lexical-let
                            ((parse (,(apply 'parser-curry-production 'start 'parser-or
                                        (mapcar 'parser-compile-definition definition))
                                      )))
                            (if parse
                              ;; if we have a production return the position at which the
                              ;; parser stopped along with the AST.
                              (cons (car parser-position) (cdr parse))
                              nil))
                          )))
                   )))
      (if (stringp compiled)
        ;; error path, print a message and return nil.
        (progn
          (message "parser-compile failed! %s" compiled)
          nil)

        ;; success path, bind the compiled parser to the parser symbol and return t.
        (progn
          (fset parser (eval compiled))
          t))
      )))

(provide 'parser)
