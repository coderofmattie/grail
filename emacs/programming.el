;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------
(eval-when-compile
  (require 'grail-profile))

;; load base profiles first
(use-grail-profiles 0 "code-highlighting" "code-editing" "code-formatting")

(use-grail-profiles 1 "lisp")

;; not done yet: "clojure"
(use-grail-profiles 2 "emacs-lisp" "common-lisp" "sql" "scheme" "perl")

;; load profiles that may depend on base profiles
(use-grail-profiles 3 "template" "slime")

;;----------------------------------------------------------------------
;;                       misc.
;;----------------------------------------------------------------------

;; found this on emacs-wiki , all scripts are automatically made executable.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t)

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


;; 2008-10-25 : ? Deprecated I think. a relic of the CVS 23.x days
(auto-composition-mode 0) ;; 23.x this is buggy, definitely for 23.0.60

;;----------------------------------------------------------------------
;;                          GUD
;;----------------------------------------------------------------------

(setq                     ;; cmd window + src
  gdb-show-main t)

;;----------------------------------------------------------------------
;;                           VC
;;----------------------------------------------------------------------

(require 'vc)

;; VC - I wish Version Control did foo ... oh wait, it's already bound
;; VC.

;; I use SubVersion as a master repository, and bzr as my sandbox
;; to create clean bisect atmoic changesets. Change the order
;; of the VC backends so bzr, svn are the first canidates for
;; registering files.

;; make sure the creatures from the black lagoon, CVS/RCS are dead last.

(setq
  vc-handled-backends `(Bzr SVN Git Hg Arch SCCS Mtn CVS RCS)
  vc-delete-logbuf-window t
  ;; just to be sure, this default may change in the future.
  vc-make-backup-files nil)

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

  ediff-window-setup-function 'ediff-setup-windows-plain

  ediff-auto-refine t)

(defun teardown-ediff-after-merge ()
  ;; cleanup ediff without asking or keeping the variants.
  (let
    ((merge ediff-buffer-C)
     (panel (current-buffer)))

    (ediff-janitor nil nil)

    (switch-to-buffer merge)
    (delete-other-windows)

    (kill-buffer panel) ))

(add-hook 'ediff-quit-hook 'teardown-ediff-after-merge)

;; this is insanely great. It displays the function you are "in" in terms
;; of the point. Really nice for reading long functions.

(which-function-mode)

(defun configure-for-buffer-ring ( buffer-ring-mode )
  (when (buffer-ring-add buffer-ring-mode)
    (local-set-key (kbd "<M-tab>")    'buffer-ring-cycle)

    (local-set-key (kbd "<M-right>")  'buffer-ring-next-buffer)
    (local-set-key (kbd "<M-left>")   'buffer-ring-prev-buffer)

    (local-set-key (kbd "<M-up>")   'buffer-torus-next-ring)
    (local-set-key (kbd "<M-down>") 'buffer-torus-prev-ring)) )

(defun configure-for-programming ( list-fn-signatures &optional buffer-ring-mode )
  "Enable my programming customizations for the buffer"

  (turn-on-font-lock)                       ;; enable syntax highlighting

  (configure-for-buffer-ring buffer-ring-mode)

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  ;; it is *really* handy to see just the function signatures of all the
  ;; functions defined in a buffer. It is so useful that every programming
  ;; mode needs to define a function so that it is bound to a key.
  (when list-fn-signatures
    (local-set-key (kbd "C-c c s") list-fn-signatures))

  ;; for starters this will comment the region, but a toggle command needs
  ;; to be defined.
  (local-set-key (kbd "C-c c ;") 'comment-region))

(defun configure-for-navigation ( forwards backwards )
  (local-set-key (kbd "M-f") forwards)
  (local-set-key (kbd "M-b") backwards))

(defun configure-for-evaluation ( eval-define eval-expression eval-region eval-buffer )
  "Enable my programming customizations for the buffer

   eval-def     : this should evaluate definitions, variables,constants, and functions.
   eval-expr    : evaluate the current or last expression
   eval-region  : evaluate the region

   C-c e     will be the prefix for evaluation functions:

   C-c e d : evaluate a define
   C-c e e : evaluate last or current sexp
   C-c e r : evaluate the region.
   C-c e b : evaluate the buffer.
  "

  ;; These bindings use C-c <char> which is reserved for the user so
  ;; I should be OK here.

  (local-set-key (kbd "C-c e d") eval-define)
  (local-set-key (kbd "C-c e e") eval-expression)
  (local-set-key (kbd "C-c e r") eval-region)
  (local-set-key (kbd "C-c e b") eval-buffer))


(defun configure-for-debugging ( eval-debug )
  (local-set-key (kbd "C-c d d") eval-debug))

(defun configure-for-macros ( expand-macro )
  (local-set-key (kbd "C-c m e") expand-macro) )

;; IRC which I use almost exclusively for #emacs
(eval-after-load 'erc
  '(progn
     (add-hook 'erc-mode-hook 'swap-paren-keys t)))

;;----------------------------------------------------------------------
;; shell-script
;;----------------------------------------------------------------------
(defconst shell-function-regex "function")

(defun shell-list-fn-signatures ()
  (interactive)
  (occur shell-function-regex))

(add-hook 'sh-mode-hook
  (lambda ()
    (configure-for-programming 'shell-list-fn-signatures "shell-mode")
    (setq sh-indentation 2) )
  t)

;;----------------------------------------------------------------------
;; C/C++ common
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.c$"       . c-mode)
				("\\.cc$"      . c++-mode)
				("\\.cpp$"     . c++-mode)
				("\\.h$"       . c++-mode)
                                 ) auto-mode-alist ))

(defun c-list-fn-signatures ()
  (interactive)
  (message "not implemented yet"))

(defun c++-list-fn-signatures ()
  (interactive)
  (message "not implemented yet"))

(add-hook 'c-mode-common-hook
  (lambda ()
    (c-set-style "linux")                 ;; base off of linux style
    (setq c-basic-offset 2)               ;; tabs are 2 spaces

    (c-set-offset 'substatement-open '0)  ;; hanging braces

    ;; auto-hungry newline and whitespace delete
    (c-toggle-auto-hungry-state 1))
  t)

(add-hook 'c-mode-hook
  (lambda ()
    (configure-for-programming 'c-list-fn-signatures "c-mode"))
  t)

(add-hook 'c++-mode-hook
  (lambda ()
    (configure-for-programming 'c++-list-fn-signatures "c++-mode"))
  t)

;;----------------------------------------------------------------------
;; Java
;;----------------------------------------------------------------------
(eval-after-load 'java-mode
  '(require 'flymake))

;;----------------------------------------------------------------------
;; xml/html/css
;;----------------------------------------------------------------------
(setq
  css-indent-offset 2)

(require 'nxml-mode)

(setq auto-mode-alist (append '( ("\\.html$"    . nxml-mode)
                                 ("\\.xhtml$"   . nxml-mode)
                                 ("\\.xml$"     . nxml-mode)
                                 ) auto-mode-alist ))

(add-hook 'nxml-mode-hook
  (lambda ()
    (dwim-tab-localize-context 'nxml-complete)
    (turn-on-dwim-tab 'nxml-indent-line)
    (configure-for-buffer-ring "nxml-mode"))
  t)

;;----------------------------------------------------------------------
;; logging
;;----------------------------------------------------------------------

(defun load-logging-file (log-file)
  (switch-to-buffer (find-file-noselect log-file))
  (rename-buffer (concat "logwatch: " log-file))

  (auto-revert-tail-mode)
  (setq buffer-read-only t)
  (goto-char (point-max)) )
