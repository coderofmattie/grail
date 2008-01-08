;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;;----------------------------------------------------------------------

;; ->Summary

;; parser.el aims to be a lightweight implementation of parsing.

;; My requirements involve building/extracting a data structure from
;; analysis of a given text. For this purpose a "top-down" parse
;; makes it easier to assemble a data structure. [TODO: why is it easier
;; this way ?]

;; ->Related Works

;; Much of the parsing facilities that I have seen so far in Emacs
;; have been various forms of regex tables.

;; There are many reasons why Emacs parsing tools would evolve away
;; from the usual parser generator tool-set.

;; 1. The requirement to work incrementally: parse errors are not
;;    errors, the user just has not finished typing :)

;; 2. Emacs analysis is often partial by requirement, usually only
;;    significant parts of the text need to be identified for
;;    features.

;; 3. Emacs can leverage the annotation facilities of
;;    text properties and overlays making analysis extremely
;;    modular and robust while editing.

;; CEDET.
;;
;; CEDET appears to be a parser generator implemented on-top of a
;; CLOS emulation. The fact that this sort of design is not "lispy"
;; does not negate it's value, but limits it IMHO. This tool-set
;; has also "grown around" the problem of analyzing source code as
;; a project which is bloat for simpler requirements.

;; ->Concept

;; A parser compiler that combines lexical (regex) analysis with LL
;; parsing compiled by recursive macro expansion.

;; The concept started out as an idea to build a parser as nested cond
;; forms. The matches would go in the predicate part of the clause
;; while the action part would go in the body.

;; This idea is expressed here with cond forms mutated into lambdas so
;; that previously defined matches can be referenced by productions.

;; -> Requirements

;; 1. Easy to use for trivial problems.

;; 2. Data structures are orthogonal to the parser and not hard-wired
;;    in. Leafs and nodes of the AST are constructed with beginning
;;    and ending positions of the text in the buffer.
;;
;;    This allows the user to choose between positions, markers, overlays
;;    according to the requirements.

;; -> Naming Conventions

;; parser-*          internal functions
;; parser-build-*    Construct the AST structures returned by the parser.
;; parser-interp-*   expand definitions of terminals and non-terminals into lisp
;; parser-compile-*  compile the expanded definitions into interchangeable match functions.

;; -> Key data structures

;; Matches

;; matches are a cons pair of ( parser . AST ).

;; Parser is used internally to indicate match success or failure. It
;; should be the distance matched from the current parser position.
;; It can, and often will be zero for a successful match. nil indicates
;; a failure to match.

;; AST is the data returned from the match ( production . ( start . end ) | list )
;; the car of the pair (production) is the production's symbol.

;;----------------------------------------------------------------------
;; diagnostics
;;----------------------------------------------------------------------

(defun parser-expr-diagnostic ( form ) ;; tested
  (format "type(%s) %s" (symbol-name (type-of form)) (pp (eval form))))

(defmacro parser-diagnostic ( form from expected ) ;; tested
  "syntax: (parser-diagnostic form from expected)

   Where form is the expr received, from is the component issuing the diagnostic,
   and expected is a message describing what the component expected"
  `(concat (format "[%s] expected: " ,from)  ,expected " not: " ,(parser-expr-diagnostic form)))

;;----------------------------------------------------------------------
;; AST constructors
;;----------------------------------------------------------------------

(defun parser-build-token ( identifier ) ;; tested
  "parser-make-token is a built-in constructor that records the analysis
   and the location of the text (id . (begin . end))"
  (cons identifier (cons (match-beginning 0) (match-end 0))))

;;----------------------------------------------------------------------
;; compiler internals
;;----------------------------------------------------------------------

;; make-match and get-match implement a table of productions and
;; tokens. Both production and tokens are interchangeable as far as
;; matching and referencing is concerned. This table gives the ability
;; to reference previously defined matches in other productions.

;; match-table is dynamically scoped by the macro, and does not appear
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

(defun parser-make-anon-func ( sexp ) ;; tested
  "bind an un-evaluated anonymous function to an un-interned symbol"
  (let
    ((anon-func (make-symbol "parser-lambda")))
    (fset anon-func (eval sexp))
    anon-func))

(defun parser-get-match ( symbol ) ;; tested
  "return the compiled match for symbol, or throw a semantic-error if it does not
   exist"
  (lexical-let
    ((existing-name (symbol-name symbol)))

    (unless (intern-soft existing-name match-table)
      (throw 'semantic-error (format "unkown match %s" existing-name)))

    (intern existing-name match-table)))

;;----------------------------------------------------------------------
;; compiler run-time
;;----------------------------------------------------------------------

;; these functions are defined independent of the compilation of a parser
;; for simplicity, and to avoid wasteful duplication in the macro expansion.

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

;; parser combination operators

;; the combination operators group matches by and,or operators as the
;; underlying mechanics of non-terminal productions. As these
;; functions are not compiled into the generated parser they have no
;; knowledge of the symbol associated with the match list, only the
;; match list itself.

(defun parser-or ( &rest match-list )
  "Combine match objects by or where the first successful match is returned.
   nil is returned if no matches are found"
  (catch 'match
    (dolist (match match-list)
      (lexical-let
        ((match-result (funcall match)))
        (if (car match-result)
          (progn
            (parser-advance (car match-result))
            (throw 'match (cons 0 (cdr match-result)))))
        ))
    (throw 'match (cons nil nil)) ;; this is what failure looks like :)
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

                  (if (car match-result)
                    (progn
                      ;; on match advance the parser and return the AST
                      (parser-advance (car match-result))
                      (cdr match-result))

                    ;; on fail backtrack and return nil
                    (progn
                      (parser-backtrack)
                      (throw 'backtrack nil)))
                  ))
              match-list)
            (parser-pop)
            )))
   (if (car ast)
     ;; would be nice to run map-filter-nil on ast.
     (cons 0 ast)
     (cons nil nil))
   ))

;;----------------------------------------------------------------------
;; syntax interpretation
;;----------------------------------------------------------------------

;; interpretation as expansion of a form into lisp is separated from
;; compilation to make the parser easier to debug and verify by
;; phase.

;; the token interpreter was split into two functions to isolate the
;; flexibility of tokens (user functions or return values for
;; constructing AST) from the hard-wired parser part.

(defun parser-interp-token-action ( identifier constructor ) ;; tested
  "Translate the AST constructor definition for a token into Elisp."

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
  "Translate a token definition into a parser match function."

  (lexical-let
    ((identifier  (car syntax))
     (regex       (cadr syntax))    ;; eval ? many regexs are stored in variables.
     (constructor (caddr syntax)))

    `(lambda ()
       (if (looking-at ,regex)
         (cons (parser-next) ,(parser-interp-token-action identifier constructor))
         (cons nil nil)
         ))
    ))

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

;;----------------------------------------------------------------------
;; compilation
;;----------------------------------------------------------------------

;; after definitions are interpreted they are evaluated or compiled and
;; stored as match functions.

(defun parser-compile-token ( syntax ) ;; tested
  "compile a token definition into a match object"
  (parser-make-match (car syntax) (parser-interp-token syntax)))

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
         (if (car result)
           (cons (car result) (cons ',identifier (cdr result)))
           result)
           ))
      ))

(defmacro parser-compile-production ( combine-function production-list )
  "parser-compile-production simplifies the syntax of interpreting and compiling
   a production. The construct looks hairy because it combines two operations
   with quoting necessitated by apply. This macro mechanizes the tricky part
   to enhance code readability."
  `(apply 'parser-curry-production
     (car ,production-list)
     ',combine-function
     (mapcar 'parser-interp-production (cdr ,production-list))))

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

        ((throw 'syntax-error (parser-diagnostic term
                                "parser definition"
                                "definition keyword token|or|and")))
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
                            (if (car parse)
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
