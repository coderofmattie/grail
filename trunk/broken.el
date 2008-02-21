;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------

;; CURRENT

;; I don't have a return phase, it's currently split imbetween the logic phase,
;; the lexical scope, and the generator. I think I need to separate the return
;; phase. The possibility of an operator going in to the return phase is significant.

;; making lambda optional for the parser entry point

;; optional returns match success when a match fails.

;; where does the stuff like optional ? the matching logic go ? before or
;; after the conditional ? putting the logic before the conditional could
;; seriously screw up things like backtracking, but maybe that is a good thing.

;; the logic should influence ast consumption. otherwise the grammar would
;; possibly have semantics that do not paralell the definition. This is
;; actually a real bug I have sniffed out here, since it is currently in
;; the conditional which is wrong. Replace transform with logic.


;; I am looking at a higher level than the code generation switches. I
;; am looking at my three parser effects, match effect, logical
;; effect, and ast effect. I also need a return effect.

;;----------------------------------------------------------------------
;; old stuff.
;;----------------------------------------------------------------------

;; also need the initial case, what if parse tree is not bound in the
;; function scope ?  or should it just always be bound ? the case of
;; returning it without consuming it is the start case, so we are back
;; to attach or return, depending on wether parse-tree is
;; bound. vunderbar.

;; I mean, what happens when a single token is the grammar ? it or reduces
;; down to a single token, and then what ? putting a consume in the match
;; has no impact on ast-new-production. unless.

;; second, is it possible for the function to just make up ast ? is
;; that why I am worried to some extent about consumption ? in that
;; case a function to function matchup might not work right ?

;; NEXT STEP

;; then write the simplification part, building a shift reduction that transforms
;; conflicts into nested functions. If the generated parser functions are essentially
;; referentially transparent by virtue of doing the right thing when nested then
;; the shift-reduce can eliminate all errors while possibly performing non-intuitive
;; simplifications.

;; dumping the intermediate table after all updates, and then the generated
;; function

;; recovery mechanism: create a parser function that is an or of all
;; the terminals in the grammar.  keep moving the input pointer by one
;; until such a sync is found. In fact recover could just be an or of
;; non-terminals.

;; FOCUS:

;; The first phase is to simply build the validator and think out the
;; semantics properly. that should get me to a working generator. The
;; next step after that is to create black box tests that verify that
;; the semantics are preserved.  then combination testing of some sort
;; could be created.

(defun parser-function-simplify ( gen-predicate match-list &rest statements )
  "The three main divisions are input consumption, matching logic, and AST.
   This interface makes primitives in these three domains essentially
   orthogonal.

     match-before

     [lexical]

     production =


     logical      ;; the logical part must go here, before the function rvalue,
                  ;; but after trap-fail.

     ;; if there was a function closure it would go here. The strangeness
     ;; of setting up and tearing down the match environment more than
     ;; once would have to be resolved.

     trap-fail    ;; unless match-fail is caught inside the predicate
                  ;; or match closure the failure inside the match environment
                  ;; will go straight to the function match logic.

     consumption  ;; if consumption is required, it goes inside the match environment.
                  ;; the whole idea of it is wrong I think. In fact consumption is moving to the
                  ;; predicates. this whole thing is just a issue because of the typing, delaying
                  ;; input consumption only allows me to distinguish production tokens. I can
                  ;; simply do a flag now.

     [match-environment
      match-consumption.

         ;; match closure is delayed evaluation of the predicate in the match
         ;; environment typically. having the same domain effects in after
         ;; and conditional is suicide. what if they both pop ?

         ;; what this boils down to is something like match-ahead needs to cancel
         ;; the gen-match-effect of popping. a domain effect can be in conditional
         ;; or unconditional, but not both. that is total crap.

         ;; a domain is converted by conditionalizing. so matchahead becomes.

         ;; domain effects need to be mutually exclusive ;; resulting in a new
         ;; function.

         ;; keep ast-new-root as the name, add a new flag called ast-production
         ;; that does the attach instead of merge.

         ;; ast-new-root changes the default rvalue.

         match-closure <- (match-predicate ?match-list))) ]

     match-after ;; if you want to mess with the environment residue before
                 ;; rvalue, then here ya go. this is used by matchahead
                 ;; which is going to throw input consumption away regardless.

     function rvalue
   "

;;----------------------------------------------------------------------
;; misc.
;;----------------------------------------------------------------------

;; This would be very cool if I had a better font for greek symbols.

;; from: http://www.emacswiki.org/cgi-bin/emacs-en/PrettyLambda

(defun pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
    nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                        ,(make-char 'greek-iso8859-7 107))
                 nil))))))

(defun instrument-function-at-call ()
  "instrument the function indicated by the call at the point. Warning, will eval the defun."
  (interactive)
  (save-excursion
    (find-function)
    (edebug-defun)))

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

;; (defun my-test ()
;;  "foo"
;;  (interactive)
;;  (if (skip-over-properties (next "\(\)") (face ".*comment.*" ".*string.*" ".*doc.*"))
;;    (message "it worked !")
;;    (message "it failed !")
;;    ))

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


(save-lexical-closure foo
  (message "red is %s" red)
  (message "blue is %s" blue)

  (push "foo" list)
  (push "bar" list)

  (message
    (if (string-equal red "hot")
      "definitely hot!"
      "bad boom!"))

  t)

(save-lexical-closure foo
  (if (string-equal red "hot")
    (progn
      (message "hot path")
      t)
    (progn
      (message "cold path")
      nil)))

(defun parser-sugar-semantics ( instructions )
  (apply 'append
    (mapcar
      (lambda (i)
        (lexical-let*
          ((primitive (if (consp i) (car i) i))
           (data      (if (consp i) (eval (cadr i))))
           (expand    (gethash primitive parser-semantic-sugar)))

          (cond
            ((functionp expand) (funcall 'expand data))
            ((listp expand)     expand)
            ((list i))) ))
      instructions)))

;; it's kind of a radical idea, but i could create a stack of the
;; deferred interpreter states including pending instruction
;; sequences. could be just the magic needed.

;; there is a stack. pops of a lazy-deferred are compiled into the parent
;; compile. That way nils are automatically ignored, but construction resumes
;; naturally.
(defun parser-semantic-interpreter-compile ( lazy-deferred &rest instructions )
  (lexical-let
    ((compiled nil)
     (semantics (when lazy-defered
                  (car lazy-deferred)))

     (tape
       (if lazy-deferred
           (append
             (parser-sugar-semantics instructions)
             (cdr lazy-deferred))

           nil)
       (parser-sugar-semantics instructions)))

    (unless semantics
      (setq semantics (copy-closure parser-function-semantics)))


    (setq lazy-deferred (parser-function-reduce semantics tape))


  ))








