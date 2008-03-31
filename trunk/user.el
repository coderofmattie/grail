;;----------------------------------------------------------------------
;; mattie.el
;; written by: Mike Mattie
;;
;; My configuration for versions 22.1 and 23.x
;;
;; this file is designed to be robust unlike load.el which should
;; fail if basic assumptions such as load-path cannot be established.
;;
;; the code in this file should depend only on reliable features
;; distributed with emacs. Any third party or risky features should
;; be loaded via load-style.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; undistributed features.
;;----------------------------------------------------------------------

(use-styles "xml" "complete" "spell" "tab")

;; (load-style "torus.el" "M-Torus will not be available - cycling extremely degraded.")

;;----------------------------------------------------------------------
;;                    General Modifications
;;----------------------------------------------------------------------

;; disable customization, automatic persistence of configuration changes.
;; I personally don't like customize as I prefer emacs to start with
;; a state I have personally defined and reviewed.

;; this line is a nasty way of disabling customize, simply specify the
;; customize file as /dev/null.

(setq custom-file "/dev/null")

(setq make-backup-files nil)            ;; backups, currently off until fixed.

(setq
  case-fold-search t
  current-language-environment "ASCII")

(setq require-final-newline t)                ;; some programs fail without a newline terminator

;;----------------------------------------------------------------------
;; associate major modes with file extensions.
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.txt$"     . text-mode)
                                 ) auto-mode-alist ))

;;----------------------------------------------------------------------
;; Emacs Server
;;----------------------------------------------------------------------

(require 'server)

(setq
  server-use-tcp t)                     ;; when tcp is turned on a auth file
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

    ;; I rarely want to quit eshell. when I do I can use quit. map
    ;; the usual kill-buffer keybinding to rid-window.
    (local-set-key (kbd "C-x k") 'rid-window) ))

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

;; (allout-init 'report)
;; not sure I like this. I can always use allout-minor-mode when I need
;; it.

;;----------------------------------------------------------------------
;; alpha features.
;;----------------------------------------------------------------------

;; load in-development features. if it is partial or completely broken
;; use broken.el
(load-user-elisp "alpha.el")

(load-user-elisp "programming.el")

