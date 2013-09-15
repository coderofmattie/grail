;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------

(defvar templates-enabled nil
  "a boolean for when the template system has been loaded.")

(defun templates-enabled-p ()
  "templates-enabled-p

   return non-nil when templates are enabled.
  "
  (when templates-enabled t))

(eval-when-compile
  (require 'grail-profile))

(use-grail-profiles 0 "lisp" "code-formatting" "sql")

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

(require 'vc-bzr)

;; enable verbose output for bzr
(push "-v" vc-bzr-log-switches)

;; support for drafting changelogs in a commit-changelog.txt file.

;(require 'commit-file)
;(activate-commit-file)

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

(require 'submerge)

;;----------------------------------------------------------------------
;; working-copy
;;----------------------------------------------------------------------
(require 'working-copy)

;; this is insanely great. It displays the function you are "in" in terms
;; of the point. Really nice for reading long functions.

(which-function-mode)

;; some mundane asthetics and keybindings plus whatever dwim input
;; expansion I can cook up.

(defun configure-for-programming ( list-fn-signatures )
  "Enable my programming customizations for the buffer"

  (turn-on-font-lock)                     ;; enable syntax highlighting

  (mattie-tab-switching)                  ;; my personal tab key setup.

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (local-set-key (kbd "M-f") 'forward-sexp)
  (local-set-key (kbd "M-b") 'backward-sexp)

  ;; it is *really* handy to see just the function signatures of all the
  ;; functions defined in a buffer. It is so useful that every programming
  ;; mode needs to define a function so that it is bound to a key.
  (local-set-key (kbd "C-c c s") list-fn-signatures)

  ;; for starters this will comment the region, but a toggle command needs
  ;; to be defined.
  (local-set-key (kbd "C-c ; r") 'comment-region))

(defun configure-for-evaluation ( eval-define eval-expression eval-region eval-buffer )
  "Enable my programming customizations for the buffer

   eval-def     : this should evaluate definitions, variables,constants, and functions.
   eval-expr    : evaluate the current or last expression
   eval-region  : evaluate the region

   C-c e     will be the prefix for evaluation functions:

   C-c e d : evaluate a define
   C-c e e : evaluate last or current sexp
   C-c e r : evaluate the region.
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

(defun swap-paren-keys ()
  "bind the parentheses to the brace keys, while the shifted
   paren keys become the braces."
  (interactive)

  ;; make the parentheses a bit easier to type, less shifting.
  (local-set-key (kbd "[") (lambda () (interactive) (insert-char ?\( 1 nil)))
  (local-set-key (kbd "]") (lambda () (interactive) (insert-char ?\) 1 nil)))

  (local-set-key (kbd "(") (lambda () (interactive) (insert-char ?\[ 1 nil)))
  (local-set-key (kbd ")") (lambda () (interactive) (insert-char ?\] 1 nil))) )

;; IRC which I use almost exclusively for #emacs
(eval-after-load 'erc
  '(progn
     (add-hook 'erc-mode-hook 'swap-paren-keys t)))

;;----------------------------------------------------------------------
;; Emacs Lisp
;;----------------------------------------------------------------------
;; basic settings
(setq
  lisp-indent-offset 2)

(defun elisp-list-fn-signatures ()
  (interactive)
  (occur "(defun"))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (swap-paren-keys)

    (configure-for-programming 'elisp-list-fn-signatures)

    ;; this binding is very important. normal evaluation of defuns such as defvar
    ;; and defcustom do not change the default value because the form only sets
    ;; the value if nil.

    ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
    ;; this function/keybinding should be used exclusively to avoid frustrating
    ;; errors.

    (configure-for-evaluation 'eval-defun 'eval-last-sexp 'eval-region 'eval-buffer)
    (configure-for-debugging 'edebug-defun) )
  t)

;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------

;; the atom definition is tweaked for regex purpose. Without including
;; the list symbols the regex would run over lists in it's quest for
;; whitespace.

(defconst scheme-regex-whitespace "[ \n\t]" "whitespace in scheme")
(defconst scheme-regex-atom "[^ \n\t()]+" "whitespace in scheme")

(defconst scheme-function-regex (concat
                                  "("
                                  scheme-regex-whitespace "*"
                                  "define"
                                  scheme-regex-whitespace "+"
                                  "("
                                  scheme-regex-whitespace "*"
                                  scheme-regex-atom
                                  scheme-regex-whitespace "+"))

(defun scheme-list-fn-signatures ()
  (interactive)
  (occur scheme-function-regex))

;; the actual scheme-mode comes from scheme. the cmuscheme, and quack are layered on top
;; of scheme mode. scheme can get loaded from an auto-mode-alist hit, so make sure that
;; when scheme mode is loaded, that cmuscheme gets layered on too.

(eval-after-load 'scheme
  '(require 'cmuscheme))

(eval-after-load 'cmuscheme
  '(progn
     (defvar scheme-scratch-buffer "*eval-scheme*" "the name of the scheme scratch buffer")
     (defvar default-scheme-interpreter "mzscheme")

     (defun scheme ()
       (interactive)

       (split-window-to-buffers
         (or (get-buffer scheme-scratch-buffer)
             (let
               ((scheme-eval-buffer (get-buffer-create scheme-scratch-buffer)))

               (with-current-buffer scheme-eval-buffer
                 (scheme-mode)
                 (setq scheme-buffer "*scheme*"))
               scheme-eval-buffer))

         (or (get-buffer "*scheme*")
           (save-excursion
             (condition-case nil
               ;; try and switch to the scheme buffer moving the point to the end of the buffer.
               (switch-to-scheme t)
               (current-buffer)
               (error
                 (let
                   ((pop-up-windows nil))
                   (run-scheme default-scheme-interpreter)
                   (current-buffer)))))) ))

     (defun scheme-send-buffer ()
       (interactive)
       "send the buffer to the scheme interpreter"
       (save-excursion
         (mark-whole-buffer)
         (scheme-send-region)))

     (add-hook 'inferior-scheme-mode-hook 'swap-paren-keys t)

     ;; since the scheme mode bindings rely on the cmuscheme goodies don't put the hook
     ;; in until after cmuscheme has loaded.
     (add-hook 'scheme-mode-hook
       (lambda ()
         (swap-paren-keys)
         (configure-for-programming 'scheme-list-fn-signatures)
         (configure-for-evaluation
           'scheme-send-definition
           'scheme-send-last-sexp
           'scheme-send-region
           'scheme-send-buffer)
         ;; (configure-for-macros     'scheme-
         )
       t) ))

;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------
(setq
  auto-mode-alist (append '(("\\.pl$"      . cperl-mode)
                            ("\\.pm$"      . cperl-mode)
                             ) auto-mode-alist ))

(defconst perl-function-regex "sub")

(defun perl-list-fn-signatures ()
  (interactive)
  (occur perl-function-regex))

(eval-after-load 'cperl-mode
  '(progn
    (setq
      cperl-invalid-face (quote off)   ;; disable trailing whitespace highlighting with _
      cperl-pod-here-scan nil          ;; more attempts to speed up font-lock

      cperl-indent-parens-as-block t   ;; This was a critical fix , no more
                                       ;; data structure indenting to the opening brace

      cperl-indent-level 2             ;; indentation adjustments
      cperl-continued-statement-offset 2)

    (add-hook 'cperl-mode-hook
      (lambda ()
        (configure-for-programming 'perl-function-regex)
        (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point))
      t) ))

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

(eval-after-load 'cc-mode
  '(progn
     (add-hook 'c-mode-common-hook
       (lambda ()
         (c-set-style "linux")                 ;; base off of linux style
         (setq c-basic-offset 2)               ;; tabs are 2 spaces

         (c-set-offset 'substatement-open '0)  ;; hanging braces

         ;; auto-hungry newline and whitespace delete
         (c-toggle-auto-hungry-state 1) )
       t)

     (add-hook 'c-mode-hook
       (lambda ()
         (configure-for-programming 'c-list-fn-signatures))
       t)


     (add-hook 'c++-mode-hook
       (lambda ()
         (configure-for-programming 'c++-list-fn-signatures))
       t) ))

;;----------------------------------------------------------------------
;; Java
;;----------------------------------------------------------------------
(eval-after-load 'java-mode
  '(require 'flymake))

;;----------------------------------------------------------------------
;; Lua
;;----------------------------------------------------------------------

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist ))

(defun lua-list-fn-signatures ()
  (interactive)
  (message "not implemented yet"))

(eval-after-load 'lua-mode
  '(add-hook 'lua-mode-hook
     (lambda ()
       (configure-for-programming 'lua-list-fn-signatures))
     t))
