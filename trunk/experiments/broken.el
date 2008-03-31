;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------

;; CURRENT

;; making lambda optional for the parser entry point

;; optional returns match success when a match fails.

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

;; recovery mechanism: create a parser function that is an or of all
;; the terminals in the grammar.  keep moving the input pointer by one
;; until such a sync is found. In fact recover could just be an or of
;; non-terminals.

;; FOCUS:

;;----------------------------------------------------------------------
;; misc.
;;----------------------------------------------------------------------

(defun dedicated-p ()
  (window-dedicated-p (get-buffer-window (current-buffer))))

(defun is-ded ()
  (interactive)
  (message
    (if (dedicated-p)
      "yes"
      "no")))

(defun get-target ()
  (interactive)
  (message "target is %s" (merc-target)))

;; (when merc-dedicate-source
;;   (setq special-display-regexps
;;     (append
;;       '(".*/wc$"       (same-window . t))
;;       '(".*/checkout$"  (same-window . t))
;;       '(".*/merge$"     (same-window . t))
;;       special-display-regexps)))

;; (setq special-display-regexps nil)


;; This would be very cool if I had a better font for greek symbols.

;; from: http://www.emacswiki.org/cgi-bin/emacs-en/PrettyLambda

(functionp (intern "get-target"))

(defun pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
    nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                        ,(make-char 'greek-iso8859-7 107))
                 nil))))))

(examine-library)

(find-function-noselect (read "repl"))
(is-ded)

;;----------------------------------------------------------------------
;;          tags source code indexing
;;----------------------------------------------------------------------

;; this may be obsolete with how cedet does a database of multiple files.

(with-current-buffer "*scratch*"
  (set (make-local-variable 'bar) 20))

  
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

;; there is a edebug function that does this. But I will hang on to the code
;; because I became quite handy at locating a function body during this proccess.
(defun edebug-called-at-point ()
  "Assuming that the point is over a symbol with a function definition instrument the
   function for debugging with edebug."
  (interactive)
  (lexical-let
    ((status (catch 'terminate
               (save-excursion
                 (let*
                   ((func (function-called-at-point))
                    (location (find-function-noselect func)))

                   (unless (consp location) (throw 'terminate t))

                   (with-current-buffer (car location)
                     (goto-char (+ (cdr location) 1))
                     (edebug-defun)))
                 nil)) ))

    (if status (message "failed! could not find the function at the point"))))

(defun path-join (list)
  (concat
    (car list) ":"
    (if (cdr list) (string-join (cdr list)))
    ))

;; I really like this implementation, would map-filter-nil benefit from
;; using consp ?

;; could use remq 'nil list
(defun list-filter-nil ( list )
  "Filter nil symbols from a list"
  (if (consp list)
    (lexical-let
      ((head (car list)))

      (if (eq head 'nil)
        (list-filter-nil (cdr list))
        (cons head (list-filter-nil (cdr list)))
        ))
    nil))

;; example posted to help-gnu-emacs

(setq my-warnings '("-Wall"))
(setq my-optimization '("-O2"))

(defun create-compile-command ( target source )
  (string-join " " (cons
                     (or (getenv "CC") "gcc")
                     (append my-warnings my-optimization (list "-o" target source)))))

(create-compile-command "foo" "foo.c")

(defun my-compile ()
  (interactive)
  (lexical-let*
    ((source    (file-name-nondirectory buffer-file-name))
     (target    (file-name-sans-extension source)))

    (if (file-readable-p (concat (file-name-directory buffer-file-name) "Makefile"))
      (compile)
      (let
        (compile-command (create-compile-command))
        (compile))) ))





