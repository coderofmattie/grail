;;----------------------------------------------------------------------
;; user.el
;; written by: Mike Mattie
;;
;; the code in this file should depend only on reliable features
;; distributed with emacs. Any third party or risky features should
;; be loaded via load-style.
;;----------------------------------------------------------------------

(eval-when-compile
  (require 'grail-groups)
  (require 'grail-fn))

;;----------------------------------------------------------------------
;; undistributed features.
;;----------------------------------------------------------------------

;; (use-grail-groups "xml" "spell" "tab")
;;
(use-grail-groups 0 "xml" "web")
(use-grail-groups 1 "tab")

;;----------------------------------------------------------------------
;;                    General Modifications
;;----------------------------------------------------------------------

;; disable customization, automatic persistence of configuration changes.
;; I personally don't like customize as I prefer emacs to start with
;; a state I have personally defined and reviewed.

;; this line is a nasty way of disabling customize, simply specify the
;; customize file as /dev/null.

(setq custom-file "/dev/null")

;; basic settings

(setq
  ;; backups are a poor substitute for revision control
  make-backup-files nil

  ;; protect against IO errors by writing to a temp file and then renaming
  ;; so the original is not trashed by partial writes.
  file-precious-flag t

  ;; set ascii.
  current-language-environment "ASCII"

  ;; keep woman from making a frame.
  woman-use-own-frame nil

  ;; some programs fail without a newline terminator
  require-final-newline t

  ;; when traversing sexp's ignore comments
  parse-sexp-ignore-comments t

  ;; default to UNIX line terminators
  default-buffer-file-coding-system 'undecided-unix

  ;; files can have elisp statements that are executed when the
  ;; file is loaded. My paranoia says hell no.
  enable-local-eval nil

  ;; when a buffer is toggled to read only enter view mode
  view-read-only t)

;; for increased security create a temporary directory and set
;; temporary-file-directory to that.
(setq temporary-file-directory (make-temp-file "emacs" t))

;;----------------------------------------------------------------------
;; associate major modes with file extensions.
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.txt$"     . text-mode)) auto-mode-alist ))

;;----------------------------------------------------------------------
;; Emacs Server
;;----------------------------------------------------------------------

;; only need to start it when Emacs is not running in daemon mode
(unless (daemonp)
  (require 'server)

  (setq
    server-use-tcp t)                     ;; when tcp is turned on a auth file
                                          ;; is created - root can use
                                          ;; emacsclient to connect to
                                          ;; a session running as
                                          ;; an unprivelaged user.

  (condition-case nil                    ;; start the server, trapping errors.
    (server-start)
    (error
      (message "cannot start Emacs Server")
      nil)) )

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
    (ansi-color-for-comint-mode-on)

    (setq show-trailing-whitespace nil)   ;; disable trailing whitespace highlighting

    ;; make up-down go through the history list.
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input)
    )
  t)

;;----------------------------------------------------------------------
;;                    Term
;;----------------------------------------------------------------------

(require 'term)

(add-hook 'term-mode-hook
  ;; disable trailing whitespace for terminal emulation
  (lambda ()
    (setq show-trailing-whitespace nil))
  t)

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
                                    (kill-buffer pbuffer))
  t)

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
	(append eshell-visual-commands (list "ssh" "su" "telnet"
                                             "ftp" "lftp" "links")))

      ;; I rarely want to quit eshell. when I do I can use quit. map
      ;; the usual kill-buffer keybinding to rid-window.
      (local-set-key (kbd "C-x k") 'rid-window))
    t))

;;----------------------------------------------------------------------
;; network protocols
;;----------------------------------------------------------------------
(setq                   ;; customize ffap to open urls in a buffer
  ffap-url-fetcher 'visit-url)

;;----------------------------------------------------------------------
;;                    Tramp remote access
;;----------------------------------------------------------------------
(eval-after-load 'tramp
  '(progn
     (setq tramp-default-method "scp2")
     (setq tramp-terminal-type "eterm-color")))

;;----------------------------------------------------------------------
;;                    ERC
;;----------------------------------------------------------------------

;; emacs IRC client is handy when on #emacs ...

(eval-after-load 'erc
  '(progn
     (setq
       erc-default-server "irc.freenode.net"
       erc-default-port "6667"
       erc-nick "codermattie")

    ;; turn on truncate mode before erc eats all available RAM.
     (require 'erc-truncate)
     (erc-truncate-mode 1)))

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
    (add-hook 'kill-buffer-hook 'rid-window t t))
  t)

;;----------------------------------------------------------------------
;;                 Structured Document Tools
;;----------------------------------------------------------------------

;; (require 'allout)

;; (add-hook 'text-mode-hook
;;   (lambda ()
;;     ;; the allout mode keybindings are found with C-c C-h
;;     (allout-mode)
;;     ))

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
(load-user-elisp "programming")

;;----------------------------------------------------------------------
;; Beta features
;;----------------------------------------------------------------------

;; beta features are commands that are in development, usable, but
;; require some more work for completion.

(load-user-elisp "beta")

