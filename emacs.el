(load-file "/usr/share/emacs/site-lisp/site-gentoo.el")

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
  '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "grey70")))))

(setq backup-file-dir (concat (getenv "HOME") "/" "edit-backups"))

;;;----------------------------------------------------------------------
;;; gnus
;;;----------------------------------------------------------------------
(setq gnus-select-method '(nntp "news.gmane.org"))

;;;----------------------------------------------------------------------
;;;                    General modifications
;;;----------------------------------------------------------------------

(blink-cursor-mode -1)		      ;;; turn off the blinking cursor

(server-start)                        ;;; start emacs-server

(display-time)                        ;;; display the time on the modeline

(add-hook 'term-mode-hook
  (lambda ()
    (set-face-foreground 'term-default-fg "grey")))

;;;
;;; shared functionality
;;;

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

(load "gtags")                            ;;; global tags mode

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

;;; configure autoloading of a mode by file extension patterns

(setq auto-mode-alist (append '(
				 ("\\.c$"       . c-mode)
				 ("\\.cc$"      . c++-mode)
				 ("\\.cpp$"     . c++-mode)
				 ("\\.h$"       . c++-mode)
				 ("\\.pl$"      . perl-mode)
				 ("\\.xsl$"     . xsl-mode)
				 ("\\.html$"    . html-helper-mode)
				 ("\\.scheme$"  . scheme-mode))
			auto-mode-alist ))

;;;
;;; elisp
;;;

(setq lisp-indent-offset 2)

(add-hook 'emacs-lisp-mode-hook '
  (lambda ()
    (turn-on-font-lock)
    (hs-minor-mode)
    (filladapt-mode 1)
    (local-set-key (kbd "<tab>") 'indent-or-complete) ;;; tab-complete
					              ;;; everything
    )
  )

;;;
;;; scheme
;;;

(setq scheme-program-name "scheme48")    ;;; setup the emacs front-end for the
                                         ;;; scheme48 virtual machine
(autoload 'run-scheme
  "cmuscheme48"
  "Run an inferior Scheme process."
  t)

(add-hook 'scheme-mode-hook              ;;; tweak scheme-mode
  (lambda ()
    (turn-on-font-lock)
    (filladapt-mode 1)))

;;;
;;; C/C++ common
;;;

(require 'ediff)		          ;;; 2-3 way merge tool, can be used
					  ;;; for cherry picking and splitting

(require 'cc-mode)                        ;;; cc-mode foundation for
					  ;;; code editing

(add-hook 'c-mode-common-hook '
  (lambda () 
    (turn-on-font-lock)            ;;; syntax highlighting
    (c-set-style "linux")          ;;; base off of linux style

    (setq c-basic-offset 2)               ;;; tabs are 2 spaces
    (c-set-offset 'substatement-open '0)  ;;; hanging braces

    (c-toggle-auto-hungry-state 1) ;;; auto-hungry newline and
				   ;;; whitespace delete

    (c-setup-filladapt)            ;;; adaptive fill for maintaing
				   ;;; indenting inside comments
    (filladapt-mode 1)             

    (hs-minor-mode)                ;;; activate hide/show mode

    (local-set-key (kbd "<tab>") 'indent-or-complete) ;;; tab-complete
				                      ;;; everything
    (gtags-mode 1)                 ;;; gtags mode
    ))
