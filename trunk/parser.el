;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie
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

;; 4. CEDET.
;;
;;    CEDET appears to be a parser generator implemented on-top of a
;;    CLOS emulation. The fact that this sort of design is not "lispy"
;;    does not negate it's value, but limits it IMHO. This tool-set
;;    has also "grown around" the problem of analyzing source code as
;;    a project which is bloat for simpler requirements.

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

;; ->Conventions

;; parser-build-*
;;
;; Construct the data structures returned by the parser.

;; parser-compile-*
;;
;; Compile the parser syntax.

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
;; compiler
;;----------------------------------------------------------------------
(defun parser-compile-action ( identifier constructor )
  "compile the action part of a parse clause consisting of an identifier
   and a constructor for AST."

  (unless (symbolp identifier)
    (throw 'syntax-error (parser-diagnostic identifier
                           "parser compile token"
                           "identifier: An unbound symbol used as an identifier"
                           )))
  (cond
    ((eq nil constructor) `(parser-build-token ',identifier))
    ((functionp constructor) `(,constructor (match-beginning) (match-end)))

    ;; all other constructor types are unhandled.
    (throw 'syntax-error (parser-diagnostic identifier
                           "parser token: identifier" "A symbol")))
  )

(defun parser-compile-token ( syntax )
  "compile a token into a match function"

  (lexical-let
    ((identifier (car syntax))
     (regex (cadr syntax))
     (constructor (cddr syntax)))

    `(lambda ()
       (if (looking-at ,regex)
         (cons t ,(parser-compile-action identifier constructor))
         (cons nil nil)
         ))
    ))
