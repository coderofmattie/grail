;;----------------------------------------------------------------------
;; emacs.el
;; written by: Mike Mattie
;;
;; configuration for versions 22.1 and 23.x
;;
;; this file is designed to be robust unlike adapt.el which should
;; fail if basic assumptions such as load-path cannot be established.
;;
;; the code in this file should depend only on reliable features
;; distributed with emacs. Any third party or risky features should
;; be guarded with load-guard.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; basic startup tuning.
;;----------------------------------------------------------------------

(setq inhibit-splash-screen t)                ;; definitely not a fan of the splash screen.
(transient-mark-mode nil)                     ;; not a big fan of transient mark mode.

(setq
  case-fold-search t
  current-language-environment "ASCII")

(toggle-uniquify-buffer-names)                ;; more intelligent unique buffer names, will automatically
                                              ;; simplify buffer names when collisions are reduced.

;;======================================================================
;;         Phase 2: Visual Asthethics & Global Key Bindings
;;======================================================================

;; disable things I don't use from eating screen-space

(tool-bar-mode -1)                            ;; cannot be set with setq
(scroll-bar-mode -1)                          ;; disable the scrollbar
(menu-bar-mode -1)                            ;; disable the menu bar as well

(message "%s" "init Phase: 1 complete")

(setq require-final-newline t)             ;; some programs fail without a newline terminator

;; the cursor
(blink-cursor-mode -1)
(set-cursor-color "yellow")

;; mode-line customization
(display-time)                            ;; display the time on the modeline

(column-number-mode 1)		          ;; handy guides when following
(line-number-mode 1)			  ;; errors

(simple-set-theme user
  ;; default
  (default splice (:stipple nil
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
                    :family "Courier New"))

  ;; comments are set off-tempature to distingiush them better.
  ;; orange was chosen as a red that wasn't harsh.
  (font-lock-comment-face foreground "orange3")

  ;; language syntax is the darkest shade of blue
  (font-lock-keyword-face foreground "DeepSkyBlue4")
  (cperl-nonoverridable-face foreground "DeepSkyBlue4")

  ;; grammar is the lightest shade of blue
  (font-lock-builtin-face foreground "SkyBlue3")

  (paren-face-match-light background "grey20")
  (paren-face-match foreground "red")

  ;; this should be for any form of literal value in code medium contrast.
  (font-lock-string-face  foreground "grey50")
  (font-lock-constant-face foreground "grey50")

  ;; decl is dark green
  (font-lock-type-face foreground "green4")
  (font-lock-function-name-face foreground "aquamarine4")
  (font-lock-variable-name-face foreground "aquamarine3")

  ;; perl misc
  (cperl-array-face slant 'italic)
  (cperl-hash-face  slant 'italic)

  ;; flyspell.
  (flyspell-incorrect underline t)
)

;; load the keybindings.
(load-config "keybinding.el")

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

;; this file is suppost to be for stable versions of the functions only.
;; stable functions are both mature and documented.
(load-config "mattie.el")

;;----------------------------------------------------------------------
;;                    General Modifications
;;----------------------------------------------------------------------

(setq make-backup-files nil)            ;; backups, currently off until fixed.

(require 'server)

(setq
  server-use-tcp t)                     ;; when tcp is turned on a auth file
                                        ;; is created - root can use emacsclient
                                        ;; to connect to a session running as
                                        ;; an unprivelaged user.
(server-start)

(setq                   ;; customize ffap to open urls in a buffer
  ffap-url-fetcher 'visit-url)

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

(setq shell-prompt-pattern "*? *")        ;; comint-prompt-regex is set
                                          ;; to this regex that captures
                                          ;; my shell prompt.

(setq
  comint-prompt-read-only t)              ;; make everything before the prompt RO

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

(setq
  eshell-windowed t                ;; enable windowing
  eshell-save-history-on-exit nil) ;; kill the prompt to save history

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
    (local-set-key (kbd "C-x k") 'rid-window)
    ))

;;----------------------------------------------------------------------
;;                    Tramp remote access
;;----------------------------------------------------------------------

(require 'tramp)

(setq tramp-default-method "scp2")
(setq tramp-terminal-type "eterm-color")

;;----------------------------------------------------------------------
;;                    ERC
;;----------------------------------------------------------------------

;; emacs IRC client is handy when on #emacs ...

(eval-after-load "erc"
  (setq
    erc-default-server "irc.freenode.net"
    erc-default-port "6667"
    erc-nick "codermattie"
    ))

;;----------------------------------------------------------------------
;;                           Diff
;;----------------------------------------------------------------------

(require 'diff)

(setq
  diff-switches "-U3")                 ;; turn on standard context diffs,

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

;;----------------------------------------------------------------------
;; alpha features.
;;----------------------------------------------------------------------

;; load in-development features. if it is partial or completely broken
;; use broken.el
(load-config "alpha.el")

;;----------------------------------------------------------------------
;; undistributed features.
;;----------------------------------------------------------------------

(load-guard "xml.el" "nxml not be available")
(load-guard "complete.el" "icicles not be available - minibuffer extremely degraded.")
(load-guard "spell.el" "enhanced spell-check not available, must run spell-check manually.")

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
      jit-lock-stealth-time 10)          ;; refontify 10 seconds after no input


(auto-composition-mode 0) ;; 23.x this is buggy, definitely for 23.0.60

;;----------------------------------------------------------------------
;;                          GUD
;;----------------------------------------------------------------------

;; (require 'speedbar)                       ;; the speedbar is handy for GUD

(setq                     ;; cmd window + src
  gdb-show-main t)

;;----------------------------------------------------------------------
;;                          Ediff
;;----------------------------------------------------------------------

;; I will enventually create my interactive merge interface with this
;; mode as a foundation. Until then tweak the basics.

(require 'ediff)		          ;; 2-3 way merge tool, can be used
					  ;; for cherry picking and splitting

(setq
  ediff-custom-diff-options "-U3"         ;; same for ediff

  ediff-split-window-function 'split-window-horizontally
  ediff-merge-split-window-function 'split-window-horizontally

  ediff-use-toolbar-p nil              ;; doesnt work ? disable the toolbar in ediff
  ediff-window-setup-function 'ediff-setup-windows-plain ;; this should work.
  )

;;----------------------------------------------------------------------
;;                          tune-programming
;;----------------------------------------------------------------------

;; latest and greatest template facility.
(load-guard "template.el" "XML enhanced else template mode not available.")

;; this is insanely great. It displays the function you are "in" in terms
;; of the point. Really nice for reading long functions.

(which-function-mode)

;; some mundane asthetics and keybindings plus whatever dwim input
;; expansion I can cook up.

(defun set-default-register ( register )
  "set the default register"
  (interactive "cregister? ")
  (set default-register register)
  )

(defun tune-programming ( lang )
  "Enable my programming customizations for the buffer"
  (interactive "Mlanguage? ")

  (turn-on-font-lock)                     ;; enable syntax highlighting

  ;; (turn-on-filladapt-mode)                ;; smart comment line wrapping

  (apply-my-keybindings lang)               ;; use my keybindings

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (set (make-local-variable 'source-language) lang)

  ;; create a default register that shortens repeated
  ;; register commands
  (set (make-local-variable 'default-register) 'a)

  ;; use Ctrl-l as the prefix for e commands. It's short
  ;; and the usual unix meaning of centering a screen is
  ;; a small loss.
  (local-unset-key (kbd "C-l"))

  (local-set-key (kbd "M-f") 'forward-sexp)
  (local-set-key (kbd "M-b") 'backward-sexp)

  (local-set-key (kbd "C-l s") 'set-default-register)
  (local-set-key (kbd "C-l r") 'list-registers)

  (local-set-key (kbd "C-l w")
    (lambda ()
      (interactive)
      (set-register default-register
        (filter-buffer-substring (region-beginning) (region-end)))
      ))

  (local-set-key (kbd "C-l i")
    (lambda ()
      (interactive)
      (insert-register default-register)))

  (else-xml-init)
)

;;----------------------------------------------------------------------
;; elisp
;;----------------------------------------------------------------------
(setq lisp-indent-offset 2)

(load-guard "paren.el" "parentheses highlighting will not be available")

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (tune-programming "elisp")

    ;; make the parentheses a bit easier to type, less shifting.
    (local-set-key (kbd "[") (lambda () (interactive) (insert-char ?\( 1 nil)))
    (local-set-key (kbd "]") (lambda () (interactive) (insert-char ?\) 1 nil)))

    (local-set-key (kbd "(") (lambda () (interactive) (insert-char ?\[ 1 nil)))
    (local-set-key (kbd ")") (lambda () (interactive) (insert-char ?\] 1 nil)))

    ;; this binding is very important. normal evaluation of defuns such as defvar
    ;; and defcustom do not change the default value because the form only sets
    ;; the value if nil.

    ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
    ;; this function/keybinding should be used exclusively to avoid frustrating
    ;; errors.
    (local-set-key (kbd "C-x e") 'eval-defun)

    ;; replace dired at point, far less useful to me than instrumenting a function.
    (local-set-key (kbd "C-x d") 'edebug-defun)

    ;; elisp doesn't need tags, find-function works just fine.
    (local-set-key (kbd "M-.") 'find-function)
    ))

;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(setq
  auto-mode-alist (append '(("\\.pl$"      . cperl-mode)
                             ("\\.pm$"      . cperl-mode)
                             ) auto-mode-alist )

  else-mode-xml-alist (cons '("perl5" . ("perl5.xml"
                                          "loop.xml"))
                        else-mode-xml-alist)

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

    (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point)
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
