;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;;----------------------------------------------------------------------

;; ->Summary

;; parser.el aims to be a lightweight implementation of parsing.

;; My requirements involve building/extracting a data structure from
;; analysis of a given text. For this purpose a "top-down" parse
;; makes it easier to assemble a data structure. [TODO why ?]

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

;; A parser compiler that combines lexical (regex) analysis with
;; parsing built by a recursive macro.

;; The compiled form is essentially a tree of nested cond forms.

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
;; parser-runtime-*  runtime functions
;; parser-build-*    Construct the AST structures returned by the parser.
;; parser-interp-*   expand definition syntax into lisp
;; parser-compile-*  compile the expanded definitions into functions.

;;----------------------------------------------------------------------
;; diagnostics
;;----------------------------------------------------------------------

(defun parser-expr-diagnostic ( form )
  (format "type(%s) %s" (symbol-name (type-of form)) (pp form)))

(defmacro parser-diagnostic ( form from expected )
  "syntax: (parser-diagnostic form from expected)

   Where form is the expr received, from is the component issuing the diagnostic,
   and expected is a message describing what the component expected"
  `(concat (format "[%s] expected: " ,from)  ,expected " not: " ,(parser-expr-diagnostic form)))

;;----------------------------------------------------------------------
;; AST constructors
;;----------------------------------------------------------------------

(defun parser-build-token ( identifier )
  "parser-make-token is a built-in constructor that records the analysis
   and the location of the text (id . (begin . end))"
  (cons identifier (cons (match-beginning 0) (match-end 0))))

;;----------------------------------------------------------------------
;; compiler internals
;;----------------------------------------------------------------------

(defun parser-make-match ( symbol function )
  "parser-make-match takes ( symbol function ) and returns a symbol
   stored in the parser's match-table with the evaluated lambda
   bound"
  (lexical-let
    ((new-name (symbol-name symbol)))

    (if (soft-intern new-name match-table)
      (throw 'semantic-error (format "illegal redefinition of match %s" new-name)))

    (lexical-let
      ((compiled-match (intern new-name match-table)))
      (fset compiled-match (eval function))
      (compiled-match)
    )))

(defun parser-get-match ( symbol )
  "return the compiled match for symbol, or throw a semantic-error if it does not
   exist"
  (lexical-let
    ((existing-name (symbol-name symbol)))

    (unless (soft-intern new-name match-table)
      (throw 'semantic-error (format "unkown match %s" existing-name)))

    (intern existing-name match-table)))

;;----------------------------------------------------------------------
;; compiler runtime
;;----------------------------------------------------------------------

(defun parser-pos ()
  "return the current position of the parser in the buffer"
  (car parser-position))

(defun parser-push ()
  "copy the parser position to a new stack level"
  (setq parser-position (cons (car parser-position) parser-position)))

(defun parser-pop ()
  "copy the parser position to a previous stack level"
  (let
    ((current (parser-pos)))
    (setq parser-position (cons (car parser-position) (cddr parser-position)))
    ))

(defun parser-backtrack ()
  "restore the previous parser position"
  (setq parser-position (cdr parser-position)))

(defun parser-advance ( distance )
  "add distance to the parsing position without changing the stack level"
  (setq parser-position (cons (+ distance (car parser-position)) (cdr parser-position))))

(defun parser-next ()
  "compute the next position for the parser as the length of the match plus one"
  (+ 1 (- (match-end 0) (match-beginning 0))))

(defun parser-or ( match-list )
  "return the match pair of the first match object that indicates a match in the text,
   nil is returned if no matches are found"

  (catch 'terminate
    (dolist (match match-list)
      (lexical-let
        ((match-result (funcall match)))
        (if (car match-result)
          (progn
            (parser-advance (car match-result))
            (throw 'terminate match-result)))
        ))
    ))

(defun parser-and ( match-list )
  (parser-push (parser-pos))

  (catch 'terminate
    (mapcar
      (lambda (match)
        (lexical-let
          ((match-result (funcall match)))

          (if (car match-result)
            (progn
              ;; ?matches need to compensate for this calculation
              (parser-advance (+ 1 (car match-result)))
              (cdr match-result))

            (progn
              (parser-backtrack)
              (throw 'terminate match-result)))

    (dolist (match match-list)

        ))
    ))
  )

;;----------------------------------------------------------------------
;; syntax interpretation
;;----------------------------------------------------------------------

;; interpretation as expansion of a form into lisp is separated from
;; compilation to make the parser easier to debug.

(defun parser-interp-token-action ( identifier constructor )
  "assemble the AST constructor for a token"

  (unless (symbolp identifier)
    (throw 'syntax-error (parser-diagnostic identifier
                           "parser token"
                           "identifier: An unbound symbol used as an identifier"
                           )))
  (cond
    ((eq nil constructor) `(parser-build-token ',identifier))
    ((functionp constructor) `(,constructor (match-beginning 0) (match-end 0)))

    ;; all other constructor types are un-handled.
    (throw 'syntax-error (parser-diagnostic identifier
                           "parser token: identifier" "A symbol")))
  )

(defun parser-interp-token ( syntax )
  "assemble a token definition into a match object"

  ;; syntax issues: it may be necessary to specify sub-captures in regex's
  ;; this particular feature is deferred for now.
  (lexical-let
    ((identifier (car syntax))
     (regex (cadr syntax))
     (constructor (cddr syntax)))

    `(lambda ()
       (if (looking-at ,regex)
         (cons (parser-next) ,(parser-interp-token-action identifier constructor))
         (cons nil nil)
         ))
    ))

(defun parser-interpret-definition ( syntax )
  (mapcar
    (lambda ( statement )
      (cond
        ((listp statement) (parser-compile-definition statement))
        ((symbolp statement) (parser-get-match statement))
        (throw 'syntax-error
          (parser-daignostic statement
            "interpret definition"
            "expected a definition as a list, or a symbol as a production/token reference"))
        ))
    syntax))

(defun parser-compile-definition ( definition )
  (dolist (term definition)
    (unless (listp term)
      (throw 'syntax-error (parser-diagnostic term
                             "parser definition"
                             "expected a definition token|first|term")))
    (lexical-let
      ((keyword (car term))
        (syntax (cdr term)))

      (cond
        ((eq keyword 'token) (parser-compile-token syntax))
        ((eq keyword 'first) (parser-compile-term
                               (car syntax) 'parser-or (parser-interpret-definition (cdr syntax))))
        ((eq keyword 'term)) (parser-compile-term
                               (car syntax) 'parser-and (parser-interpret-definition (cdr syntax)))
        ))
    ))

;;----------------------------------------------------------------------
;; compilation
;;----------------------------------------------------------------------

;; after definitions are interpreted they are evaluated or compiled and
;; stored as match functions.

(defun parser-compile-token ( syntax )
  "compile a token definition into a match object"
  (parser-make-match (car syntax) (parser-interp-token syntax)))

(defun parser-compile-term ( identifier combine-operator grammar )
  "compile a term into a match object"
  (unless (symbolp identifier)
    (parser-diagnostic identifier
      "compile term"
      "match identifier"))

  (parser-make-match identifier (eval `(lambda ()
                                         (,combine-operator ,@grammar)))))

(defmacro parser-compile ( &rest definition )
  "compile a LL parser from the given grammar definition"
  (let
    ((match-table (make-vector LENGTH 0))) ;; create a symbol table to store
                                           ;; compiled terminal and non-terminal match functions

    `(lambda ( start-pos )
       (let
         ((parser-position (cons start-pos nil))) ;; initialize the backtrack stack
         (save-excursion
           (goto-char start-pos)
           ,(parser-compile-term 'start 'parser-or (parser-compile-definition definition)))
         ))

    ;; compile the grammar to the start match.
    ))
