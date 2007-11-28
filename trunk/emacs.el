;;----------------------------------------------------------------------
;; emacs.el
;; written by: Mike Mattie
;;
;; configuration for versions 22.1 and 23.x
;;----------------------------------------------------------------------

;;======================================================================
;;         Phase 1: Library loading and host init/normalization
;;======================================================================

;;----------------------------------------------------------------------
;; String handling functions oriented towards manipulating path lists.
;; These are essential for the earliest part of the init process,
;; modifying the library loading path.
;;----------------------------------------------------------------------

(defun string-join (prefix list)
  ;; This is analgous to the perl5 join function.
  ;; given a <prefix> and a <list> of strings join the
  ;; strings with <prefix> as a seperator between the
  ;; list values.
  ;;
  ;; The result is a single string value.
  (concat
    prefix (car list)
    (if (cdr list) (string-join prefix (cdr list)))
    ))

(defun string-prefix-list (prefix list)
  (cons
    (concat prefix (car list))

    (cond
      ((cdr list) (string-prefix-list prefix (cdr list)))
       ('()))
      ))

(defun path-join (list)
  (concat
    (car list) ":"
    (if (cdr list) (string-join (cdr list)))
    ))

;;----------------------------------------------------------------------
;; extend the library search space with local changes and third pary
;; extensions
;;----------------------------------------------------------------------

;; This code will assume a FS structure like this:
;;
;; $HOME/.emacs.d/            | all of emacs scribbles here by default so I treat
;;                              it like /var
;;
;; $HOME/system/emacs/emacs.el| entry point for emacs initialization
;; $HOME/system/emacs/*.el    | other libraries.

;; $HOME/system/emacs/local   | distributed files that have been locally modified
;; $HOME/system/emacs/elisp   | Third party extensions that are not distributed by
;;                              emacs and not integrated through host package management.
;;                              This is the highest maintenance burden.

(setq localized-source-dir (concat (getenv "HOME") "/system/emacs/local/"))
(setq extras-source-dir    (concat (getenv "HOME") "/system/emacs/elisp"))

;; $HOME/system/emacs/patches | patches against upstream

;; only system/emacs/local is currently placed in the path. the files in system/emacs
;; are assumed to chain manually.

;; The config files are relocated to the $HOME/system/emacs so the config/code
;; under version control is not stomped on or cluttered by all the traffic
;; into the standard location: session and intra-session state.

(setq load-path
  (append
    ;; overide distributed elisp with local modifications by
    ;; inserting a "local" directory at the beginning of the
    ;; load list
    (cons localized-source-dir load-path)

    ;; add the extras to the end of the list.
    (list extras-source-dir)
    ))

;;----------------------------------------------------------------------
;; Host specific customization.
;;
;; each host system has a customization file that normalizes the platform
;; and extends the library search space for extra libraries it manages.
;;----------------------------------------------------------------------
(cond
  ;; Gentoo has a file that tunes emacs and loads the third party
  ;; components managed by the package manager.
  ((string-equal "gnu/linux" system-type)
    (load-file "/usr/share/emacs/site-lisp/site-gentoo.el"))

  ;; on darwin assume carbon-emacs.
  ((string-equal "darwin" system-type)
    (load-file (concat (getenv "HOME") "/system/emacs/darwin.el")))
  )

;;----------------------------------------------------------------------
;; basic startup tuning.
;;----------------------------------------------------------------------

(custom-set-variables
 '(tool-bar-mode nil))                     ;; disable the toolbar ; waste of space

(setq inhibit-splash-screen t)

(custom-set-variables
  '(case-fold-search t)
  '(current-language-environment "ASCII"))

;;======================================================================
;;         Phase 2: Visual Asthethics & Global Key Bindings
;;======================================================================

(message "%s" "init Phase: 1 complete")

(custom-set-variables
  '(require-final-newline t)              ;; always require a final newline.
  '(show-paren-mode t nil (paren)))

;; the cursor
(blink-cursor-mode -1)
(set-cursor-color "yellow")

;; mode-line customization

(display-time)                            ;; display the time on the modeline

(column-number-mode 1)		          ;; handy guides when following
(line-number-mode 1)			  ;; errors

(custom-set-faces
  ;; default font.
  '(default ((t
	      (:stipple nil
	       :background "black"
	       :foreground "grey70"
	       :inverse-video nil
	       :box nil
	       :strike-through nil
	       :overline nil
	       :underline nil
	       :slant normal
	       :weight normal
	       :height 110
	       :width normal
	       :family "Courier New"))))

  '(font-lock-comment-face ((((class color) (background dark)) (:foreground "green4"))))
  '(font-lock-keyword-face ((((class color) (background dark)) (:foreground "royalblue3"))))
  '(font-lock-string-face ((((class color) (background dark)) (:foreground "grey80"))))
  '(font-lock-type-face ((((class color) (background dark)) (:foreground "grey60"))))
  '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "grey70"))))

  '(cperl-array-face ((((class color) (background dark)) (:foreground "grey70"))))
  '(cperl-hash-face ((((class color) (background dark)) (:foreground "grey70"))))
  '(cperl-nonoverridable-face ((((class color) (background dark)) (:foreground "grey70"))))
)

;;----------------------------------------------------------------------
;;                    Custom Key Bindings
;;----------------------------------------------------------------------

;; enable ffap bindings so that C-x C-f on things like include directives
;; opens the paths. This could be very magical.

(ffap-bindings)


;; M-g    = (goto-line)  |  move point to the line number in buffer

(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)

;; escape = (execute-extended-command)

;; standard emacs prompt for a interactive command

(global-set-key (kbd "<escape>") 'execute-extended-command)

;; C-xe   = (eval-expression)

;; evaluate the elisp expression given by the user

(global-set-key "\C-xe" 'eval-expression)

;; M-TAB  = switch to the last buffer in the current window. cycles when
;;          repeated.

(global-set-key [(meta tab)]
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer))
    ))

;; S-TAB  = Shift tab cycles between windows.

(global-set-key [(shift tab)] 'other-window)

(defun contextualized-tab (completion-context)
  ;; generate a contextualized flavor of the tab key behavior.

  (lexical-let
    ((completion-function
       ;; this cond should be the tail of an if special form. First
       ;; check if the completion-context symbol is a function. If
       ;; so simply lexically-bind. Otherwise interpret as a string.

       (cond
         ;; use lisp-complete-symbol for elisp
         ((string-equal completion-context "elisp") (function lisp-complete-symbol))

         ;; fall back on dabbrev
         ((lambda () (dabbrev-expand nil)))
         )))

    (lambda ()
      "Complete if point is at end of a word, otherwise indent line."
      (interactive)

      (if (looking-at "\\>")
        (funcall completion-function)
        (indent-for-tab-command))
      ))
  )

(defun apply-my-keybindings ( completion-context )
  ;; when setting up major/minor modes adapt my global key-bindings to
  ;; the modes of the buffer. Local key-bindings that shadow my global
  ;; key-bindings are unset or replaced with contextualized variations
  ;; of my global defaults.

  (local-unset-key [(meta tab)])
  (local-unset-key [(shift tab)])

  (local-set-key [(tab)] (contextualized-tab completion-context))
  )

;;--------------------------> TODO <----------------------------------

;; I was trying to map ESC to 'execute-extended-command with a single keystroke.
;; when I unmapped ESC all of the meta sequences were unbound. I would like
;; to solve this, but until then I will use C-xe which is pretty comfortable.

;; Shift-Tab cycling is not working on Carbon-Emacs

;;----------------------------------------------------------------------
;; associate major modes with file extensions.
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.txt$"     . text-mode)
				 ) auto-mode-alist ))

;;======================================================================
;;         Phase 3: Turn on more complex functionality.
;;======================================================================
(message "%s" "init Phase: 2 complete")

;;----------------------------------------------------------------------
;;                     Personal ELisp Library.
;;----------------------------------------------------------------------

(load-file (concat (getenv "HOME") "/system/emacs/mattie.el"))

;;----------------------------------------------------------------------
;;                    General Modifications
;;----------------------------------------------------------------------

(setq make-backup-files nil)            ;; backups, currently off until fixed.

(require 'server)

(custom-set-variables
  '(server-use-tcp t))                  ;; when tcp is turned on a auth file
                                        ;; is created - root can use emacsclient
                                        ;; to connect to a session running as
                                        ;; an unprivelaged user.

(server-start)

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

(setq shell-prompt-pattern "*? *")        ;; comint-prompt-regex is set
                                          ;; to this regex that captures
                                          ;; my shell prompt.

(custom-set-variables
  '(comint-prompt-read-only t)            ;; make everything before the prompt RO
  )

(add-hook 'comint-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)   ;; disable trailing whitespace highlighting

    ;; make up-down go through the history list.
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input)
    ))

(add-hook 'term-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)   ;; disable trailing whitespace
    ))                                    ;; for terminal emulation

;;----------------------------------------------------------------------
;;                    EShell
;;----------------------------------------------------------------------

(require 'eshell)

(custom-set-variables
  '(eshell-windowed t))   ;; enable windowing

(add-hook 'eshell-mode-hook
  (lambda ()
    ;; disable trailing whitespace
    (setq show-trailing-whitespace nil)

    ;; add a list of commands that will pop a term buffer for out-of-eshell
    ;; handling. Note: the variable eshell-visual-commands is buffer-local.
    (setq eshell-visual-commands
      (append eshell-visual-commands (list "ssh" "su" "telnet")))

    ;; ensure that none of my custom keybindings are affected.
    (apply-my-keybindings nil)

    ;; I rarely want to quit eshell. when I do I can use quit. map
    ;; the usual kill-buffer keybinding to rid-window.
    (local-set-key "\C-xk" 'rid-window)
    ))

;;----------------------------------------------------------------------
;;                    Tramp remote access
;;----------------------------------------------------------------------

(require 'tramp)
(setq tramp-default-method "scp2")
(setq tramp-terminal-type "eterm-color")

;;----------------------------------------------------------------------
;;                           Diff
;;----------------------------------------------------------------------

(require 'diff)

(custom-set-variables
  '(diff-switches "-U3"))                 ;; turn on standard context diffs,

(add-hook 'diff-mode-hook

  ;; when diff is called it will pop a window which is nice, but killing
  ;; the buffer did not get rid of the popped window , until now.

  (lambda ()
    (add-hook 'kill-buffer-hook 'rid-window t t)))

;;----------------------------------------------------------------------
;;                 Structured Document Tools
;;----------------------------------------------------------------------
(require 'allout)

(add-hook 'text-mode-hook
  (lambda ()
    ;; the allout mode keybindings are found with C-c C-h
    (allout-mode)
    ))

;; (allout-init 'report)
;; not sure I like this. I can always use allout-minor-mode when I need
;; it.


;;======================================================================
;;                  Phase 4: Programming
;;======================================================================
(message "%s" "init Phase: 3 complete")

;;----------------------------------------------------------------------
;;                       misc.
;;----------------------------------------------------------------------

;; found this on emacs-wiki , all scripts are automatically made executable.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;----------------------------------------------------------------------
;;                   whitespace dogma
;;----------------------------------------------------------------------

;; space vs. tab, trailing, can get into alot of trouble committing
;; "dirty" files.

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

;; make sure tabs are impossible to miss.
(global-hi-lock-mode 1)
(highlight-regexp "\t")

;;----------------------------------------------------------------------
;;                      font-lock engine
;;----------------------------------------------------------------------

;; tune font-lock, important for hairy files.
(setq jit-lock-contextually nil          ;; only refontify modified lines
      jit-lock-defer-contextually t      ;; until 5 seconds has passed
      jit-lock-stealth-time 10           ;; refontify 10 seconds after no input
      jit-lock-stealth-time 15)          ;; how long to wait to start deferred fontification


;;----------------------------------------------------------------------
;;                          GUD
;;----------------------------------------------------------------------

;; (require 'speedbar)                       ;; the speedbar is handy for GUD

(custom-set-variables                     ;; cmd window + src
  '(gdb-show-main t))

;;----------------------------------------------------------------------
;;                          Ediff
;;----------------------------------------------------------------------

;; I will enventually create my interactive merge interface with this
;; mode as a foundation. Until then tweak the basics.

(require 'ediff)		          ;; 2-3 way merge tool, can be used
					  ;; for cherry picking and splitting

(custom-set-variables
  '(ediff-custom-diff-options "-U3")      ;; same for ediff

  '(ediff-split-window-function 'split-window-horizontally)
  '(ediff-merge-split-window-function 'split-window-horizontally)

  '(ediff-use-toolbar-p nil)              ;; doesnt work ? disable the toolbar in ediff
  '(ediff-window-setup-function 'ediff-setup-windows-plain) ;; this should work.
  )


;;----------------------------------------------------------------------
;;                          tune-programming
;;----------------------------------------------------------------------
(require 'else-mode)

(setq else-mode-template-dir (concat (getenv "HOME") "/system/emacs/else/"))

(defun else-language-spec-p ( lang )
  "determine if a language definition has been loaded for lang"
  (if (assoc lang else-Language-Definitions)
    t
    nil)
  )

;; (else-language-spec-p "perl5")  - should be false
;; (else-language-spec-p "Empty")  - shoule be true

(defun minimal-else-language-def ( language-name )

  ;; create an alternative loading scheme. Instead of a language defining a complete
  ;; or base set of tokens , load only the language settings.

  ;; returns true if such a definition is already loaded, or definition file is found
  ;; and successfully loaded.

  (if (else-language-spec-p language-name)
    (return t))

  (let
    ((template-path (concat else-mode-template-dir language-name ".lse")))

    (if (not (file-readable-p template-path))
      (return nil))

    (save-excursion
      (with-temp-buffer
        (progn
          (beginning-of-buffer)
          (insert-file-contents-literally template-path nil nil nil t )
          (else-compile-buffer)
          )))
    ))

(defun tune-else ()
    (if (minimal-else-language-def source-language)
      (progn

        ;; localize the current language to the buffer and set it properly
        (set (make-local-variable 'else-Current-Language) lang)
        (else-mode)
        )
      )

  ;;else-is-template-file-present
  ;;file-readable-p
  )

(defun tune-programming ( lang )
  ;; customization shared by all programming languages.

  (turn-on-font-lock)                     ;; enable syntax highlighting

  ;; (turn-on-filladapt-mode)                ;; smart comment line wrapping

  (apply-my-keybindings lang)               ;; use my keybindings

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (set (make-local-variable 'source-language) lang)

  (tune-else)
)

;;----------------------------------------------------------------------
;; elisp
;;----------------------------------------------------------------------
(setq lisp-indent-offset 2)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (tune-programming "elisp")
    ))

;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(setq auto-mode-alist (append '(("\\.pl$"      . cperl-mode)
				("\\.pm$"      . cperl-mode)
                                 ) auto-mode-alist )

  cperl-invalid-face (quote off)   ;; disable trailing whitespace highlighting with _
  cperl-pod-here-scan nil          ;; more attempts to speed up font-lock

  cperl-indent-parens-as-block t   ;; This was a critical fix , no more
                                   ;; data structure indenting to the opening brace

  cperl-indent-level 2             ;; indentation adjustments
  cperl-continued-statement-offset 2
  )

(add-hook 'cperl-mode-hook
  (lambda ()
    (tune-programming "perl5")

    (local-set-key "\C-hf" 'cperl-perldoc-at-point)
    ))

;;----------------------------------------------------------------------
;; C/C++ common
;;----------------------------------------------------------------------

(require 'cc-mode)                        ;; cc-mode foundation for
					  ;; code editing
(setq auto-mode-alist (append '(
				 ("\\.c$"       . c-mode)
				 ("\\.cc$"      . c++-mode)
				 ("\\.cpp$"     . c++-mode)
				 ("\\.h$"       . c++-mode)
				 ) auto-mode-alist ))

(add-hook 'c-mode-common-hook
  (lambda ()
    (c-setup-filladapt)            ;; adaptive fill for maintaining
				   ;; indenting inside comments

    (c-set-style "linux")          ;; base off of linux style

    (setq c-basic-offset 2)               ;; tabs are 2 spaces
    (c-set-offset 'substatement-open '0)  ;; hanging braces

    (c-toggle-auto-hungry-state 1) ;; auto-hungry newline and
				   ;; whitespace delete
    ))
