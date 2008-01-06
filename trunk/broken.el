;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; compiler
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; token syntax
;;----------------------------------------------------------------------
(defun parser-compile-action ( identifier value )
  "compile the action part of a parse clause"
  (cond
    ((symbolp identifer)
      (cond
        ;; unbound symbols are identifiers
        ((not (boundp identifer)) `(parser-build-token ,identifier))

        ;; if the symbol is a bound function pass that function
        ;; the match range.
        ((functionp identifier) `(,identifier (match-beginning) (match-end)))

        (signal syntax-error (parser-diagnostic identifier
                               "parser token: identifier"

                               "A symbol to be unbound as an
                                identifier or bound as a
                                function."))
        ))

    ;; all other types are currently un-handled
    (signal syntax-error (parser-diagnostic identifier
                           "parser token: identifier" "A symbol"))
    ))


(defun parser-compile-action ( identifier constructor )
  "compile the action part of a parse clause consisting of an identifier
   and a constructor for AST."

  (unless (and
            (symbolp identifier)
            (not (boundp identifer)))

    (signal syntax-error (parser-diagnostic identifier
                           "parser token: identifier"
                           "An unbound symbol used as an identifier"
                           )))
  (cond
    ((eq nil constructor) `(parser-build-token ,identifier))
    ((functionp constructor) `(,constructor (match-beginning) (match-end)))

    ;; all other constructor types are unhandled.
    (signal syntax-error (parser-diagnostic identifier
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
         (cons t ,@(parser-compile-action identifier constructor))
         (cons nil nil)
         ))
    ))


;; keep token around. token is what happens when it encounters a list.
;; when it encounters a symbol it figures it for a reference.

(defun parser-compile-token ( syntax )
  (lexical-let
    ((identifier (car syntax))
     (regex (cadr syntax)))
    `((looking-at ,regex) ,@(parser-compile-action identifier (cddr syntax)))
    ))

;; write the terms as (term op ) make a simple cond out of it.
;; need an operator to combine the tokens into terms

;; the difference between a term and a token is that a term is by default
;; expects a argument of a list of clauses, whereas a token expects a regex.

;; there is a implicit term or as the top-level.

;; need to compile in a term/token table for referencing tokens. what really
;; needs to be done is binding.
(defmacro parser-compile-term ( forms )
  `(lambda ( expr )
     (lexical-let
       ((syntax (car expr))
         (symbol (cadr expr))

         (cond
           ( parser-compile

       (cond
         ,(mapcar
            (lambda (form)
              `((eq ,(car form) term) (,(cdr form) expr ))

             `(looking-at ,(cadr form))

             (if (cddr form)
               (list 'cond (apply 'toke-table-exception (cddr form)) (cons (cadr form) nil))
               (car form))
             ))


  )

;;----------------------------------------------------------------------
;; syntax-dispatch
;;----------------------------------------------------------------------

  (cond
    ((eq token (car form)) (parser-compile-token form))

(defun parser-compile-term ( form )
  (parser-define-terms
    '(token . parser-compile-token))

  (cond
    ((eq token (car form)) (parser-compile-token form))

  )

(defmacro parser-compile ( &rest forms )
  "create a tokenizing table. Creating a tree of cond forms offers substantial
   analysis capabilities with a small macro."

  ;; need to establish the symbol table here.
  (append `(term or)
    (apply parser-compile-term forms))



  `(cond
     ,@(mapcar
         (lambda (form)
           (list
             `(looking-at ,(cadr form))

             (if (cddr form)
               (list 'cond (apply 'toke-table-exception (cddr form)) (cons (cadr form) nil))
               (car form))
             ))
         forms)
     ))

;;----------------------------------------------------------------------
;; experimental
;;----------------------------------------------------------------------

(defun make-parser-exception ( exception-form )
  "create a cond clause out of the exception form"

  (condition-case nil

    (syntax-error (

                    ))
    )

  (list
    ;; each of the exceptions are translated into a looking-at match
    ;; combined by or.
    (cons 'or (mapcar
                (lambda ( excep ) `(looking-at ,excep))
                (cdr exception-form)))

    ;; the symbol for the exception is the second list.
    (car exception-form)
    ))

;; stateful implementation of map-filter-nil.

;; stateful version by fledermous in #emacs (thanks)
;; (remove nil (mapcar fun (remove nil list)))

;; stateful version by sabetts in #emacs (thanks).
;;(defun map-reduce (fn &rest list)
;;  (let (acc v)
;;    (while list
;;      (setq v (pop list)
;;            v (and v (funcall v)))
;;      (when v (push v acc)))
;;    acc))

;;----------------------------------------------------------------------
;;          tags source code indexing
;;----------------------------------------------------------------------

;; this may be obsolete with how cedet does a database of multiple files.

(obsoloted 'gtags)
(defun tune-gtags ()
  (gtags-mode)

  ;; bind the global keys, f for find , r for references , and p for pop.

  (local-set-key "\C-c/f" 'gtags-find-tag)
  (local-set-key "\C-c/r" 'gtags-find-rtag)
  (local-set-key "\C-c/p" 'gtags-pop-stack)
  )

;; FROM tune-programming
(tune-gtags)

;;----------------------------------------------------------------------
;; Map modes to file formats
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '( ("\\.xsl$"     . xsl-mode)
				 ("\\.scheme$"  . scheme-mode)
				 ) auto-mode-alist ))

;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------
(setq inferior-lisp-program "/usr/bin/clisp")
;; (require 'slime)
;; (slime-setup)


;; FROM: tune-programming

;; (require 'paredit)

;;  (paredit-mode +1)

;;  (define-key paredit-mode-map (kbd "(") 'paredit-open-parenthesis)
;;  (define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis)

;;  (local-set-key "\C-c/r" 'query-replace-regexp)


;;----------------------------------------------------------------------
;; old experiments that worked but did not pan out.
;;----------------------------------------------------------------------

(defun paludis-overlay ()
  (auto-overlay-unload-regexp 'paludis)
  (auto-overlay-load-regexp
    `(word ("^\\\*[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]*$" . 1)
       (face . deploy-package-name-face)
       (paludis-type . name)
       (local-map . ,paludis-keymap)
       (read-only . t) ;; still doesn't work
       )
    'paludis
    )

  (auto-overlay-load-regexp
    `(word ("^[[:blank:]]+\\([^[:blank:]]+:\\)[[:blank:]]*" . 1)
       (face . deploy-repository-face)
       (paludis-type . name)
       (local-map . ,paludis-keymap)
       (read-only . t) ;; still doesn't work
       )
    'paludis
    )

  (auto-overlay-start 'paludis)
  )

