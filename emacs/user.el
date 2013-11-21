;;----------------------------------------------------------------------
;; user.el
;; written by: Mike Mattie
;;
;; the code in this file should depend only on reliable features
;; distributed with emacs. Any third party or risky features should
;; be loaded via load-style.
;;----------------------------------------------------------------------

(eval-when-compile
  (require 'grail-profile)
  (require 'grail-fn))

;;----------------------------------------------------------------------
;; undistributed features.
;;----------------------------------------------------------------------
(require 'dwim-tab)
(require 'buffer-ring)

(use-grail-profiles 0 "tramp" "spell" "registers" "browser" "terminal" "jabber-emacs")

(use-grail-profiles 10 "activate-buffer-status")

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

;;----------------------------------------------------------------------
;; buffer coding and line ending handling
;;
;; unix eol, utf-8 coding, and tab character insertion
;;----------------------------------------------------------------------
(setq
  buffer-file-coding-system 'utf-8-unix
  set-language-environment "UTF-8"
  set-locale-environment "en_US.UTF-8")

(setq file-coding-system-alist '( (".*" . utf-8-unix) ))

(prefer-coding-system 'utf-8-unix)

(require 'eol-utilities)

;;----------------------------------------------------------------------
;; whitespace handling. tabs are evil.
;;----------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(require 'whitespace-utilities)

;;----------------------------------------------------------------------
;; lots of defaults that will be broken out when they become more than
;; setting a single flag.
;;----------------------------------------------------------------------

(setq
  ;; turn off backups and auto-save is just plain dangerous
  backup-inhibited t
  auto-save-default nil

  ;; protect against IO errors by writing to a temp file and then renaming
  ;; so the original is not trashed by partial writes.

  ;; unfortunately this breaks hard-linking so I have been forced to
  ;; turn it off
  file-precious-flag nil

  ;; keep woman from making a frame.
  woman-use-own-frame nil

  ;; some programs fail without a newline terminator
  require-final-newline t

  ;; when traversing sexp's ignore comments
  parse-sexp-ignore-comments t

  ;; files can have elisp statements that are executed when the
  ;; file is loaded. My paranoia says hell no.
  enable-local-eval nil)

;;----------------------------------------------------------------------
;; use marmelade to get the latest and greatest stuff
;;----------------------------------------------------------------------
(eval-after-load "package"
  '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;;----------------------------------------------------------------------
;; associate major modes with file extensions.
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.txt$"     . text-mode)) auto-mode-alist ))

;;----------------------------------------------------------------------
;; Emacs Server
;;----------------------------------------------------------------------

;; only need to start it when Emacs is not running in daemon mode

(grail-trap
  "Starting the Emacs Server"

  (unless (daemonp)
    (require 'server)
    (server-start)

    (unless (server-running-p)
      (server-force-delete)
      (server-start)) ))

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

    ;; make up-down go through the history list.
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input))
  t)

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
;; ido smart completion
;;----------------------------------------------------------------------
(require 'ido)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)

(ido-mode 1)

;;----------------------------------------------------------------------
;; read/write perm handling
;;----------------------------------------------------------------------
(require 'rw-utilities)

;;----------------------------------------------------------------------
;; Beta features
;;----------------------------------------------------------------------

;; beta features are commands that are in development, usable, but
;; require some more work for completion.

(load-user-elisp "beta")
