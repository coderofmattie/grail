;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; old stuff.
;;----------------------------------------------------------------------

;; Is a functional closure runs in the lexical scope useful ?  the ast
;; append will be fine I think, but the AST generation is no longer
;; controlled by the branching logic. I can't see anything that
;; couldn't be accomplished by nesting except violating semantics.

;; Any closure stacking should result in function stacking. the point
;; of the dynamic scope is largely to show the parse tree with it's tail.


;; how do I implement attaching a node instead of merging a node ? this
;; is what descend is all about. ast-attach, ast-merge. Probably only
;; need ast-attach as an override. ast-node for attach or ast-leaves for merge

;; ast-node-attach sounds best, like ast-node-merge, object then verb.
;; ast-newroot is actually a interesting thought. along with being a root, it's
;; a place where data is injected into the parse tree. This is freely controlled
;; in tokens, but not in productions which are limited to symbols. in reality
;; it should be called ast-new-production.

;; ast-attach-production, by not stripping off the car under the assumption that
;; the car is a symbol we use to identify the production, rather than a logical
;; match status, we have the ability to create productions.

;; if it isn't a production up front then we need to assume merging.

;; also need the initial case, what if parse tree is not bound in the function scope ?
;; or should it just always be bound ? the case of returning it without consuming it
;; is the start case, so we are back to attach or return, depending on wether parse-tree is
;; bound. vunderbar.


;; ast-new-production, ast-attach-production, ast-return-production
;; okay with this table approach I need to know how it's modified. Each group that
;; is exclusive to the other creates a tag. If the tag matches it overrides the
;; default. Otherwise it signals an error.

;; shadowing the tree, basically environment modification gives the immediate
;; advantage of allowing the tail of the AST list to be tracked during parsing.
;; The lexical environment has the head, the dynamic environment has the tail.
;; This keeps the computational complexity of AST construction linear.

;; This is why a function closure would not work all that well. It would need to
;; dynamically scope a tail. best to just let it go.

;; a predicate is a match function that produces a match result in the
;; match environment.

;; a match closure can be passed the predicate for delayed evaluation.
;; this is typically used for greedy

;; a closure is essentially an optional two level deep delayed evaluation.
;; the two levels is required to directly express grouping/alternation and
;; positive/kleene closure.

;; matchahead discards the input consumption of a descent.

;; forget  discards the AST construction of a descent.

;; optional returns match success when a match fails.

;; backtrack discards both input consumption on match failure or forgets
;; a saved position on match success.

;; ast-newroot creates a new AST tree populated by the parse descent
;; that shadows the existing AST tree

;; gen-transform is applied outside of any dynamic-scoping.
;; effects and rvalue take place within a lexical-scope if any.

;; here is what I am pondering. backtracking introduces conditionals.
;; do conditionals affect all domains ? or can it be selected ?
;; are consumption and ast possible to separate conditionally ?

;; that make up ast is what I will need for the start thingy. Tokens
;; are essentially the make up ast, which means of course that I do
;; need mandatory consumption. What happens when I directly attach
;; to the match result of a token ? that means that I absolutely
;; need consumption in the match call.

;; Focus. Function to function. both simply make up return values. Where is the consumption ?
;; no where.

;; FIXME: branching implies a trap.

;; I mean, what happens when a single token is the grammar ? it or reduces
;; down to a single token, and then what ? putting a consume in the match
;; has no impact on ast-new-production. unless.

;; second, is it possible for the function to just make up ast ? is
;; that why I am worried to some extent about consumption ? in that
;; case a function to function matchup might not work right ?

;; NEXT STEP

;; generate errors for intermediate forms with conflicting side effects

;; then write the simplification part, building a shift reduction that transforms
;; conflicts into nested functions. If the generated parser functions are essentially
;; referentially transparent by virtue of doing the right thing when nested then
;; the shift-reduce can eliminate all errors while possibly performing non-intuitive
;; simplifications.

;; dumping the intermediate table after all updates, and then the generated
;; function

;; making lambda optional for the parser entry point

;; If i convert the simple dynamic scope of the table, to a hash table
;; that is dynamically scoped then I can make wrappers for query, setting,
;; and inserting operations. That will be enough I think to detect conflicts.

;; keeping the queries terse will be difficult, might ponder wrapping setting
;; only. consumption will likely have to be seperated into a different entry.
;; consumption outside of dynamic scope is a serious non-orthogonality. Other
;; transforms are harmless and can be compounded.

;; when will the function scope be restored ? the let match environment
;; looks like a restore point, but caveat emperor, the backtracking doesn't
;; restore it until func-rvalue. hmmm. This seams essential, so that the
;; branching in the function can decide what to do with the residue of the
;; match environment.

;; transform may modify the match result, but it can't do anything like
;; alter the input consumption of the match, that will happen in branching.

;; where does the stuff like optional ? the matching logic go ? before or
;; after the conditional ? putting the logic before the conditional could
;; seriously screw up things like backtracking, but maybe that is a good thing.

;; the logic should influence ast consumption. otherwise the grammar would
;; possibly have semantics that do not paralell the definition. This is
;; actually a real bug I have sniffed out here, since it is currently in
;; the conditional which is wrong. Replace transform with logic.

;; BIG IDEA: validation and generation need to be two separate steps.
;; say you have a intermediate form in a table. you have a cycle of
;; copy, merge,validate.  you keep your last validated intermediate
;; form, when you fail to validate you kick out a function and
;; nest. this way you are not generating code all the time. would
;; maybe simplify a bit.

;; I need a bind hash table function. lethash. I can even modify I think
;; if I do it right. that gives full read/write.

;; better yet use an association list. It's way better, easier to populate
;; etc. Look up time doesn't need.

;; ast-rvalue
;; ast-attach
;; ast-merge
;; ast-return

(always backtrack)
(fail   backtrack)

(ast tail 'start)   ;; creates a new ast tail initialized to 'start

;;;; WOOO

;; here we go, backtrack is a concept for ast as well !! including forget !!!


(input backtrack)
(input discard)

(ast backtrack)
(ast discard)

(logic xor)
(ast node 'start)    ;; give the node a initial value, causes parser-tree-add-node
                     ;; to be 




(func-ast)  ;; if there is a value here, ast-scope should pick it up,
(match-ast) ;; this is where things get lexically scoped. Branching is not required just
            ;; to shadow. A seperate variable is not required because the result
            ;; will show up in production, so it can be initialized as a match result
            ;; which would be merged, or func-ast-add could be 

(always rvalue)
(
(match rvalue ast) ;; this triggers ast shadowing.

;; recovery mechanism: create a parser function that is an or of all the terminals in the grammar.
;; keep moving the input pointer by one until such a sync is found. In fact recover could just be
;; an or of non-terminals.



;; FOCUS:

;; need to focus on the validator. Make the descisians more transparent with
;; some sort of defun breakdown, or tracing. Making the validator will turn
;; all of these comments into code.

;; how do I make it persistent ?

;; custom code must be domain tagged so it can be validated. It can also be
;; a user domain which doesn't collide.

;; I am looking at a higher level than the code generation switches. I am looking
;; at my three parser effects, match effect, logical effect, and ast effect. I
;; also need a return effect.

;; so I have forget, unconditionally discard ast, then I have conditional ast
;; which is implies ast-new-root, but a wank like ast-new-root without conditional
;; is doable actually. useful to delay ast consumption to the next function.

;; any conditional branching of ast effect, needs to set ast-new-root, otherwise
;; ast consumption will happen outside of conditional control.

;; binding needs to happen seperate from merging so I can call myself with dependencies.
;; 

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

  (lexical-let
    ((param-type (make-hash-table)) )

    ;; initialize the hash of parameters that are lists instead of single values.

    (mapc (lambda ( list-name )
            (puthash list-name t param-type))
      '(match-before match-after gen-match-effects gen-fail-effects))

    ;; generation table is dynamically scoped to ease breaking up
    ;; the function generator into pieces and phases.

    ;; This table represents the intermediate form of
    ;; simplification.
    (let
      ((gen-sexp          nil)

       ;; in application order top-down.
       ;; gen-predicate match-list?
       (gen-closure       nil)
       (gen-consume       nil)
       (gen-trap          nil)
       (gen-transform     nil)

        ;; gen production captures the match result to the lexically
        ;; scoped production symbol.
       (gen-production    nil)

       (gen-ast-root      nil)
       (gen-lexical-scope nil)
       (gen-dynamic-scope nil)

        ;; before/after hooks.
       (gen-match-before  nil)
       (gen-match-after   nil)

        ;; branching on match results.

       (gen-match-effects nil)
       (gen-match-rvalue  nil)

       (gen-fail-effects  nil)
       (gen-fail-rvalue   nil))

      (mapc (lambda (s)
              (catch 'next
                (unless s (throw 'next nil))

                (if (listp s)
                  (if (gethash (car s) param-type)
                    (eval `(push ,(cadr s) ,(car s)))
                    ;; FIXME: here is where the test will go for something already set.
                    (eval `(setq ,(car s) ,(cadr s))))
                  (cond
                    ((eq s 'consume) (setq gen-consume t))
                    ((eq s 'trap-fail) (setq gen-trap t))
                    ;; I just triggered branching and failed to consume on the match
                    ;; branch. not good.
                    ((eq s 'optional)
                      (setq  gen-fail-rvalue `(parser-make-production-match nil)))
                    ((eq s 'forget)
                      (progn
                        (setq gen-match-rvalue '(parser-make-production-match nil))
                        (push `(parse-tree (make-production-match nil)) gen-dynamic-scope)))
                    ((eq s 'matchahead)
                      (progn
                        (push `(parser-push) gen-match-before)
                        (push `(parser-backtrack)  gen-match-after)))
                    ((eq s 'backtrack)
                      (progn
                        (push `(parser-push)       gen-match-before)
                        (push `(parser-pop)        gen-match-effects)
                        (push `(parser-backtrack)  gen-fail-effects)))
                    ((eq s 'ast-newroot)
                      (progn
                        ;; values for gen-ast-root can be default, symbol,
                        ;; or sexp initializer.
                        (if gen-ast-root
                          (if (symbolp gen-ast-root)
                            (setq gen-ast-root `(cons ',gen-ast-root nil)))
                          (setq gen-ast-root `(parser-make-logical-match)))

                        (push `(ast-root ,gen-ast-root)  gen-lexical-scope)
                        (push `(parse-tree ast-root)     gen-dynamic-scope)
                        (unless gen-match-rvalue
                          ;; since our shadowing has detached our AST construction
                          ;; be sure to attach it. as long as it was not initialized
                          ;; with a number.
                          (setq gen-match-rvalue `(parser-?consume-match ast-root)))))
                    )))) statements)

      (if (and
            (parser-gen-branch-p)
            (not gen-trap))
        (setq gen-trap t))

      ;; What I am really looking for in in a symbol is wether it is merged
      ;; as a leaf or a node, 

      ;; when there is match after statements we always need to preserve
      ;; the return value, so that it can be returned as the final value
      ;; of the list.
      (if (and gen-match-after (not gen-production))
        (setq gen-production t))

      ;; If there is a conditional branch the production will need to
      ;; be referenced in two places so turn on production generation.
      (if (and
            (and
              (parser-gen-branch-p)
              (not gen-match-rvalue)) ;; optimize, when match-rvalue is specified
                                      ;; we can turn off automatic production generation.

            (not gen-production))
        (setq gen-production t))

      (funcall (if gen-sexp 'seq-filter-nil 'parser-gen-lambda)
        gen-match-before

        ;; can this sequence in name match the description of phases ?
        (parser-gen-lexical-scope
          (parser-gen-match-rvalue
            (parser-gen-dynamic-scope
              (parser-gen-match-call))))

        gen-match-after
        )) ))

;; This would be very cool if I had a better font for greek symbols.

;; from: http://www.emacswiki.org/cgi-bin/emacs-en/PrettyLambda

(defun pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
    nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                        ,(make-char 'greek-iso8859-7 107))
                 nil))))))

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

;;----------------------------------------------------------------------
;;
;;----------------------------------------------------------------------

(pp (macroexpand '(scope-let foo
                    (message "red is %s" red)
                    (message "blue is %s" blue))))

(pp (macroexpand '(scope-private-let foo
                    (message "red is %s" red)
                    (message "blue is %s" blue))))

(scope-private-let foo
  (message "red is %s" red)
  (message "blue is %s" blue))

(scope-copy-let
  (foo red blue)
  (message "red is %s" red)
  (message "blue is %s" blue)
  ;;(setq blue "rancid"))
  )

(scope-shared-lexical foo
  (message "red is %s" red)
  (message "blue is %s" blue)

  (setq blue "gimp")
  )

(scope-let foo
  (message "red is %s" red)
  (message "blue is %s" blue))

(setq foo (make-scope
            (red   "hot")
            (blue  "cold")))

(setq bar (copy-sequence foo))

(scope-shared-lexical baz
  (message "red is %s" red)
  (message "blue is %s" blue)

  (setq blue "freezing")
  )


(length foo)

(pp-scope bar)
(pp-scope foo)

(set (intern "baz") (symbol-value (intern "red" foo)))

(setq baz (copy-scope foo))
  
(pp-scope baz)

(setq baz 'green)

(pp foo)

(set (intern "red" foo) "shit")
(symbol-value (intern "red" foo))
(symbol-value (intern "blue" foo))


(setq foo (make-scope
            (red   "hot")
            (blue  "cold")))

(pp-scope foo)

"symbol: red = \"hot\"
symbol: blue = \"cold\"
"

(scope-shared-lexical foo
  (message "red is %s" red)
  (message "blue is %s" blue)

  (setq blue "freezing"))

(pp-scope foo)
"symbol: red = \"hot\"
symbol: blue = \"freezing\"
"

(setq stuff (parser-function-reduce parser-function-semantics
            `(predicate 'parser-predicate-and)
            `(sequence '(foo bar baz))
            'greedy))

(pp-scope stuff)

(lexical-let
  ((validate (catch 'semantic-error
               (parser-function-validate stuff))))
  (unless (eq 't validate)
    (message "invalid semantics: %s" (symbol-name validate)))

  (pp (parser-function-generate stuff)))









