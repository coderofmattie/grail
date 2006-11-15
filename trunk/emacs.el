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

(set-cursor-color "yellow")
(setq inhibit-splash-screen t)


;; TODO: no backup files as a temporary solution to having a directory
;; for backup files.

(setq make-backup-files nil)

;;;----------------------------------------------------------------------
;;; gnus
;;;----------------------------------------------------------------------
(setq gnus-select-method '(nntp "news.gmane.org"))

;;;----------------------------------------------------------------------
;;;                    General modifications
;;;----------------------------------------------------------------------

(blink-cursor-mode -1)		      ;;; turn off the blinking cursor

(gnuserv-start)                       ;;; start emacs-server

(display-time)                        ;;; display the time on the modeline

(add-hook 'term-mode-hook
  (lambda ()
    (set-face-foreground 'term-default-fg "grey")))

;;;----------------------------------------------------------------------
;;; basic programming functionality
;;;----------------------------------------------------------------------

(require 'emerge)		          ;;; 2-3 way merge tool, can be used
					  ;;; for cherry picking and splitting

(require 'filladapt)		          ;;; mode for adaptive fill (works
					  ;;; inside of comments

(global-unset-key (kbd "\M-g"))	          ;;; map alt-g to goto a line number
(global-set-key "\M-g" 'goto-line)

(column-number-mode 1)		          ;;; handy guides when following
(line-number-mode 1)			  ;;; errors


(require 'hideshow)                       ;;; hide-show minor mode for
					  ;;; collapsing and expanding
					  ;;; source code

(require 'speedbar)		          ;;; speedbar for use with the tags
				          ;;; facility

(require 'else-mode)                      ;;; load the else template system

;;; hack to auto expand a collapsed buffer when performing a goto to a line
;;; inside a collapsed region, copied from content on emacsWiki

(defadvice goto-line (after expand-after-goto-line
		       activate compile)

  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block))
  )

;;; auto-completion or identation routine

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
    (dabbrev-expand nil)
    (indent-for-tab-command)
    ))

(global-unset-key "\C-m")	          ;;; \C-m is going to be our magic
                                          ;;; key so we don't need a alias
                                          ;;; for RET
(defun coder-common ( language )
  (turn-on-font-lock)                     ;;; enable syntax highlighting
  (filladapt-mode 1)                      ;;; filladapt for formatting comments.   

  (local-set-key (kbd "<tab>") 'indent-or-complete) ;;; tab-complete
					            ;;; everything

  (gtags-mode)                            ;;; gtags mode, best tags support
  
  (local-set-key (kbd "<right>") 'else-next-placeholder)
  (local-set-key (kbd "<left>") 'else-prev-placeholder)
  
  (local-set-key "\C-m \C-e" 'else-expand-placeholder)
  (local-set-key "\C-m \C-d" 'else-kill-placeholder)

;;  (mode-compile)                                    ;;; turn on compile mode

;;  (local-set-key "\C-cc" 'mode-compile)             ;;; bind keys to compile
;;  (local-set-key "\C-ck" 'mode-compile-kill)
)

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
				 ("\\.html$"    . html-helper-mode)
				 ("\\.scheme$"  . slime-mode)
				 )
			auto-mode-alist ))

;;;----------------------------------------------------------------------
;;; perl5
;;;----------------------------------------------------------------------
(setq cperl-invalid-face (quote off))   ;;; disable trailing whitespace with _
(setq cperl-pod-here-scan nil)

(add-hook 'cperl-mode-hook '
  (lambda ()
    (coder-common "perl5")

    (else-mode)                             ;;; else language sensitive templates

    (local-set-key (kbd "C-h f") 'cperl-perldoc)

    (cperl-toggle-electric)
    (cperl-toggle-abbrev)
    ))

;;;----------------------------------------------------------------------
;;; elisp
;;;----------------------------------------------------------------------

(setq lisp-indent-offset 2)

(add-hook 'emacs-lisp-mode-hook '
  (lambda ()
    (coder-common "elisp")
    (hs-minor-mode)
    (local-set-key (kbd "<tab>") 'indent-or-complete) ;;; tab-complete
					              ;;; everything
    )
  )

;;;----------------------------------------------------------------------
;;; scheme
;;;----------------------------------------------------------------------

(setq scheme-program-name "scheme48")    ;;; setup the emacs front-end for the
                                         ;;; scheme48 virtual machine
(autoload 'run-scheme
  "cmuscheme48"
  "Run an inferior Scheme process."
  t)

(add-hook 'scheme-mode-hook              ;;; tweak scheme-mode
  (lambda ()
    (coder-common "scheme")
    ))

;;;----------------------------------------------------------------------
;;; C/C++ common
;;;----------------------------------------------------------------------

(require 'cc-mode)                        ;;; cc-mode foundation for
					  ;;; code editing

(add-hook 'c-mode-common-hook '
  (lambda () 
    (c-setup-filladapt)            ;;; adaptive fill for maintaing
				   ;;; indenting inside comments
    (coder-common "C")
    (c-set-style "linux")          ;;; base off of linux style

    (setq c-basic-offset 2)               ;;; tabs are 2 spaces
    (c-set-offset 'substatement-open '0)  ;;; hanging braces

    (c-toggle-auto-hungry-state 1) ;;; auto-hungry newline and
				   ;;; whitespace delete

    (hs-minor-mode)                ;;; activate hide/show mode

    (local-set-key (kbd "<tab>") 'indent-or-complete) ;;; tab-complete
				                      ;;; everything
    ))
