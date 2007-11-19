;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------
;;          else (L)anguage (S)ensitive (E)diting.
;;----------------------------------------------------------------------
(require 'else-mode)

(defun tune-else-language ( lang )
  (let ((resume (current-buffer))
	 (template-file (concat (getenv "HOME") "/projects/else-mode/" lang ".lse"))
	 (template-buf ))

    (set-buffer (find-file-read-only template-file))

    (beginning-of-buffer)
    (else-compile-buffer)

    (kill-buffer template-buf)
    (set-buffer resume)
    )

  ;; localize the current language to the buffer and set it properly
  (set (make-local-variable 'else-Current-Language) lang)
  (else-mode)

  ;; C-C / is my magic prefix
  (local-set-key (kbd "<right>") 'else-next-placeholder)
  (local-set-key (kbd "<left>") 'else-prev-placeholder)

  (local-set-key "\C-c/e" 'else-expand-placeholder)
  (local-set-key "\C-c/d" 'else-kill-placeholder)
  )


;; FROM the cperl-mode hook
;;    (tune-else-language "perl" )

;;    (cperl-toggle-electric)
;;    (cperl-toggle-abbrev)

;;----------------------------------------------------------------------
;; Map modes to file formats
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '( ("\\.xsl$"     . xsl-mode)
				 ("\\.html$"    . nxml-mode)
				 ("\\.xhtml$"   . nxml-mode)
				 ("\\.xml$"     . nxml-mode)
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
