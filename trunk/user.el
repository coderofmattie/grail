;;----------------------------------------------------------------------
;; user.el
;; written by: Mike Mattie
;;
;; the code in this file should depend only on reliable features
;; distributed with emacs. Any third party or risky features should
;; be loaded via load-style.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; undistributed features.
;;----------------------------------------------------------------------

(use-grail-groups "xml" "spell" "tab")

;;----------------------------------------------------------------------
;;                    General Modifications
;;----------------------------------------------------------------------

;; increase the max eval depth to 4k. Hope this doesn't croak Emacs.
(setq max-lisp-eval-depth 4096)

;; make sure that the pretty printer doesn't truncate which frustrates my
;; development.

(setq
 print-length nil
 eval-expression-print-level nil
 print-level nil)

;; disable customization, automatic persistence of configuration changes.
;; I personally don't like customize as I prefer emacs to start with
;; a state I have personally defined and reviewed.

;; this line is a nasty way of disabling customize, simply specify the
;; customize file as /dev/null.

(setq custom-file "/dev/null")

;; basic settings

(setq make-backup-files nil)            ;; backups are a poor substitute for revision control

(setq
  case-fold-search t
  current-language-environment "ASCII")

(setq require-final-newline t)                ;; some programs fail without a newline terminator

(setq default-buffer-file-coding-system 'undecided-unix)  ;; default to UNIX line terminators

;; documentation tools

(let
 ((info-archive (concat grail-local-dir "info")))

  (when (file-accessible-directory-p info-archive)
    (push info-archive Info-additional-directory-list)) )

(setq-default woman-use-own-frame nil)

;;----------------------------------------------------------------------
;; associate major modes with file extensions.
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.txt$"     . text-mode)
                                 ) auto-mode-alist ))

(fset 'yes-or-no-p 'y-or-n-p)                ;; y/n instead of yes/no

;;----------------------------------------------------------------------
;; Emacs Server
;;----------------------------------------------------------------------

(require 'server)

(setq
  server-use-tcp t)                     ;; when tcp is turned on a auth file
                                        ;; is created - root can use emacsclient
                                        ;; to connect to a session running as
                                        ;; an unprivelaged user.

(condition-case nil                    ;; start the server, trapping errors.
  (server-start)

  (error
    (message "cannot start Emacs Server")
    nil))

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

(setq shell-prompt-pattern "*? *")        ;; comint-prompt-regex is set
                                          ;; to this regex that captures
                                          ;; my shell prompt.

(setq
  comint-prompt-read-only t)              ;; make everything before the prompt RO

;; the convention of adding lambda's to hooks is discouraged as
;; re-eval'ing the buffer or byte compiling duplicates the lambda in
;; the hook list.

(add-hook 'comint-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)   ;; disable trailing whitespace highlighting

    ;; make up-down go through the history list.
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input)
    ))

;;----------------------------------------------------------------------
;;                    Term
;;----------------------------------------------------------------------

(require 'term)

(add-hook 'term-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)   ;; disable trailing whitespace
    ))                                    ;; for terminal emulation

;; setup a hook that runs when the term process exits
(defvar term-process-exit-hook nil
  "a hook run when the term process exists")

;; infect the term-sentinel function
(defadvice term-sentinel (around term-sentinel-hook (proc msg))
  (let
    ((pbuffer (process-buffer proc))
     (did-exit (memq (process-status proc) '(signal exit))))
    ad-do-it
    (when did-exit (run-hooks-with-arg term-process-exit-hook pbuffer)) ))

;; get rid of the buffer when the process exits
(add-hook 'term-process-exit-hook (lambda ( pbuffer )
                                    (kill-buffer pbuffer)))

(ad-activate 'term-sentinel)

;;----------------------------------------------------------------------
;;                    EShell
;;----------------------------------------------------------------------

(robust-load-elisp "eshell"
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
	(append eshell-visual-commands (list "ssh" "su" "telnet" "ftp" "lftp" "links")))

      ;; I rarely want to quit eshell. when I do I can use quit. map
      ;; the usual kill-buffer keybinding to rid-window.
      (local-set-key (kbd "C-x k") 'rid-window) )))

;;----------------------------------------------------------------------
;; network protocols
;;----------------------------------------------------------------------
(setq                   ;; customize ffap to open urls in a buffer
  ffap-url-fetcher 'visit-url)

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
  (progn
    (setq
      erc-default-server "irc.freenode.net"
      erc-default-port "6667"
      erc-nick "codermattie") ))

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

;;----------------------------------------------------------------------
;; crypto support via gnupg
;;----------------------------------------------------------------------
(require 'epa)
(epa-file-enable)

(setq
  epa-file-cache-passphrase-for-symmetric-encryption t
  epa-armor t)

;; (allout-init 'report)
;; not sure I like this. I can always use allout-minor-mode when I need
;; it.

;;----------------------------------------------------------------------
;;                       Programming
;;----------------------------------------------------------------------
(load-user-elisp "programming.el")

;;----------------------------------------------------------------------
;; alpha features.
;;----------------------------------------------------------------------

;; load in-development features. if it is partial or completely broken
;; use broken.el
(load-user-elisp "experiments/alpha.el")

