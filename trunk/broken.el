;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------

(defmacro skip-over (iterator &rest predicates )
  "skip a iterator over spans of text properties"
  `(lambda () (lexical-let
     ((iter (lambda () ,(list (cond
                                ((string-equal "next" (car iterator)) 'skip-chars-forward)
                                ((string-equal "prev" (car iterator)) 'skip-chars-backward))

                          (cadr iterator))
              ))
       (skip-p (lambda ()
                 (or
                   ,(mapcar (lambda (p) `(match-char-property ,@p)) predicates)
                   )))
       )
     (while (cond
              ((funcall skip-p)
                (progn
                  (,(concat (symbol-name (car iterator)) "-property-change") (point))
                  t))
              ((funcall iter) t))
       nil)
     )))


(defmacro skip-over-properties ( iterator &rest predicates )
  `(lexical-let
     ;; bind the iterator as a lambda so we can eval more than once
     ((iter (lambda ()
              ,(list
                 ;; select the correct skip-chars based on direction
                 (cond
                   ((string-equal "next" (car iterator)) 'skip-chars-forward)
                   ((string-equal "prev" (car iterator)) 'skip-chars-backward))

                 ;; get the regex delimiter in place, supply the inversion
                 ;; since skip-chars wraps given regex  in a []
                 (concat "^" (cadr iterator)))
              ))
     )
     (funcall iter)
  ))

;;                   ,(mapcar (lambda ( p )
;;                              ;;                                 (or
;;                                   ,(mapcar (lambda ( match ) ) (cdr p))
;;                                   ))) predicates)


;; another experiment
(defmacro gen-seek ( count )
  `(let
     ((iter (lambda () (goto-char (+ (point) ,count))))
       (funcall 'iter)))
  )

;;----------------------------------------------------------------------
;;          tags source code indexing
;;----------------------------------------------------------------------
;; (require 'gtags)
(defun tune-gtags ()
  (gtags-mode)

  ;; bind the global keys, f for find , r for references , and p for pop.

  (local-set-key "\C-c/f" 'gtags-find-tag)
  (local-set-key "\C-c/r" 'gtags-find-rtag)
  (local-set-key "\C-c/p" 'gtags-pop-stack)
  )

;; FROM tune-programming
(tune-gtags)

;; FROM the cperl-mode hook

;;    (cperl-toggle-electric)
;;    (cperl-toggle-abbrev)

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
