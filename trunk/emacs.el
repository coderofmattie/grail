;;;----------------------------------------------------------------------
;;; adjust to the host enviornment
;;;----------------------------------------------------------------------
(if (string-equal "gnu/linux" system-type)
    (load-file "/usr/share/emacs/site-lisp/site-gentoo.el"))

(setq load-path (cons
		 (concat (getenv "HOME") "/system/lib/elisp/") load-path))

;;;----------------------------------------------------------------------
;;; this is archaic, the custom stuff. I want to get rid of it soon.
;;;----------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
  '(case-fold-search t)
  '(current-language-environment "ASCII")
  '(require-final-newline t)
  '(show-paren-mode t nil (paren)))

(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
  '(default ((t (:stipple nil :background "black" :foreground "grey70" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 123 :width normal :family "dec terminal"))))
  '(font-lock-comment-face ((((class color) (background dark)) (:foreground "green4"))))
  '(font-lock-keyword-face ((((class color) (background dark)) (:foreground "royalblue3"))))
  '(font-lock-string-face ((((class color) (background dark)) (:foreground "grey80"))))
  '(font-lock-type-face ((((class color) (background dark)) (:foreground "grey60"))))
  '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "grey70"))))

  '(cperl-array-face ((((class color) (background dark)) (:foreground "grey70"))))
  '(cperl-hash-face ((((class color) (background dark)) (:foreground "grey70"))))
  '(cperl-nonoverridable-face ((((class color) (background dark)) (:foreground "grey70"))))
)

;;;----------------------------------------------------------------------
;;;                    asthethics
;;;----------------------------------------------------------------------

(blink-cursor-mode -1)		      ;;; turn off the blinking cursor
(set-cursor-color "yellow")
(setq inhibit-splash-screen t)

(display-time)                        ;;; display the time on the modeline


(column-number-mode 1)		          ;;; handy guides when following
(line-number-mode 1)			  ;;; errors

;;;----------------------------------------------------------------------
;;;                    General modifications
;;;----------------------------------------------------------------------

(add-to-list 'eshell-visual-commands "ssh") ;; eshell works correctly !
                                            ;; woot ! best way to remote ssh

(setq shell-prompt-pattern "*? *")        ;;; critical , fix for my shell
                                          ;;; prompts.
(add-hook 'term-mode-hook
  (lambda () (setq show-trailing-whitespace nil))) ;; disable trailing whitespace
                                                   ;; for terminal emulation

;; TODO: no backup files as a temporary solution to having a directory
;; for backup files.

(setq make-backup-files nil)

(server-start)

(require 'tramp)
(setq tramp-default-method "scp2")
(setq tramp-terminal-type "eterm-color")

(global-unset-key "\M-g")	          ;;; map alt-g to goto a line number
(global-set-key "\M-g" 'goto-line)

;;;----------------------------------------------------------------------
;;; language specific tuning
;;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(
				 ("\\.c$"       . c-mode)
				 ("\\.cc$"      . c++-mode)
				 ("\\.cpp$"     . c++-mode)
				 ("\\.h$"       . c++-mode)
				 ("\\.pl$"      . cperl-mode)
				 ("\\.pm$"      . cperl-mode)
				 ("\\.xsl$"     . xsl-mode)
				 ("\\.html$"    . nxml-mode)
				 ("\\.xhtml$"   . nxml-mode)
				 ("\\.xml$"     . nxml-mode)
				 ("\\.scheme$"  . scheme-mode)
				 )
			auto-mode-alist ))

;;;----------------------------------------------------------------------
;;;          tags source code indexing
;;;----------------------------------------------------------------------

(defun tune-gtags ()
  (gtags-mode)

  ;; bind the global keys, f for find , r for references , and p for pop.

  (local-set-key "\C-c/f" 'gtags-find-tag)
  (local-set-key "\C-c/r" 'gtags-find-rtag)
  (local-set-key "\C-c/p" 'gtags-pop-stack)
  )

;;;----------------------------------------------------------------------
;;;          else (L)anguage (S)ensitive (E)diting.
;;;----------------------------------------------------------------------
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

;;;----------------------------------------------------------------------
;;;           basic programming functionality
;;;----------------------------------------------------------------------

;;; space vs. tab, trailing, can get into alot of trouble committing
;;; "dirty" files.

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

(global-hi-lock-mode 1)
(highlight-regexp "\t")

;;; tune font-lock, important for hairy files.
(setq jit-lock-contextually nil          ;;; only refontify modified lines
      jit-lock-defer-contextually t      ;;; until 5 seconds has passed
      jit-lock-stealth-time 10           ;;; refontify 10 seconds after no input
      jit-lock-stealth-time 15)          ;;; how long to wait to start deferred fontification

(require 'emerge)		          ;;; 2-3 way merge tool, can be used
					  ;;; for cherry picking and splitting

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
    (dabbrev-expand nil)
    (indent-for-tab-command)
    ))

;; (require 'paredit)

(defun tune-programming ( lang )
  (turn-on-font-lock)                     ;;; enable syntax highlighting

  (if (string-equal "gnu/linux" system-type)
   ;;; Gnu/Linux only features.
    (progn
      (turn-on-filladapt-mode)            ;;; smart comment line wrapping
      (tune-gtags)))                      ;;; gtags mode, best tags support,

  (local-set-key (kbd "<tab>") (function indent-or-complete)) ;;; tab-complete
					                      ;;; everything
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (local-set-key "\C-c/r" 'query-replace-regexp)

  (set (make-local-variable 'source-language) lang)

;;  (paredit-mode +1)

;;  (define-key paredit-mode-map (kbd "(") 'paredit-open-parenthesis)
;;  (define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis)
)

(defun show-bad-ws()
  (highlight-regexp "\t"))

;;;----------------------------------------------------------------------
;;; repl
;;;
;;; Handy tool for explatory programming. pops a new frame with the
;;; interpreter for the language running in a REPL loop.
;;;----------------------------------------------------------------------

;;; TODO: use comint 'send-invisible' to dump a function into the interperter,
;;; for this to be really cool , it would need to copy the function by tags lookup
;;; automatically reformatting as necessary.

(defun repl ( lang )
  "start a REPL interpreter interface in a new frame based upon a given or inferred language parameter"
  (interactive "MLanguage: ")

  (select-frame-set-input-focus (make-frame))

  (cond
    ((string-equal "perl5" lang)
	(switch-to-buffer (make-comint "perl5 REPL" "/usr/bin/perl" nil "-d" "-e shell")))
    ((string-equal "elisp" lang)
      (ielm))
    (else
      (message "I don't support language %s" lang))
    ))

;;;----------------------------------------------------------------------
;;; perl5
;;;----------------------------------------------------------------------
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(setq
  cperl-invalid-face (quote off)   ;;; disable trailing whitespace with _
  cperl-pod-here-scan nil          ;;; more attempts to speed up font-lock

  cperl-indent-parens-as-block t   ;;; This was a critical fix , no more
                                   ;;; data structure indenting to the opening brace

  cperl-indent-level 2             ;;; indentation adjustements
  cperl-continued-statement-offset 2
  )

(add-hook 'cperl-mode-hook
  (lambda ()
    (tune-programming "perl5" )
    (tune-else-language "perl" )

    (cperl-toggle-electric)
    (cperl-toggle-abbrev)

    (local-set-key "\C-hf" 'cperl-perldoc-at-point)
    ))

;;;----------------------------------------------------------------------
;;; elisp
;;;----------------------------------------------------------------------
(setq lisp-indent-offset 2)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (tune-programming "elisp" )
    ))

;;;----------------------------------------------------------------------
;;; common lisp
;;;----------------------------------------------------------------------
(setq inferior-lisp-program "/usr/bin/clisp")
(require 'slime)
(slime-setup)


;;;----------------------------------------------------------------------
;;; C/C++ common
;;;----------------------------------------------------------------------

(require 'cc-mode)                        ;;; cc-mode foundation for
					  ;;; code editing
(add-hook 'c-mode-common-hook
  (lambda ()
    (c-setup-filladapt)            ;;; adaptive fill for maintaing
				   ;;; indenting inside comments

    (c-set-style "linux")          ;;; base off of linux style

    (setq c-basic-offset 2)               ;;; tabs are 2 spaces
    (c-set-offset 'substatement-open '0)  ;;; hanging braces

    (c-toggle-auto-hungry-state 1) ;;; auto-hungry newline and
				   ;;; whitespace delete
    ))
