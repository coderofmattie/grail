;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------
(require 'grail-profile)
(require 'buffer-status)

;;----------------------------------------------------------------------
;; programming packages not dependent on third party support
;;----------------------------------------------------------------------

(require 'tags-uber)
(require 'working-copy)

(require 'merging)
(require 'ext-merging)

(require 'lang-repl)

;; re-usable programming modules
(use-grail-profiles 0 "code-highlighting" "code-editing" "code-formatting" "repl")

;; higher level functionality
(use-grail-profiles 1 "lisp" "code-documentation" "version-control")

;; language profiles
;; not done yet: "clojure"
(use-grail-profiles 2 "emacs-lisp" "common-lisp" "sql" "scheme" "perl" "shell-scripting" "web")

;; advanced functionality
(use-grail-profiles 3 "template" "slime")

;;----------------------------------------------------------------------
;;                          GUD
;;----------------------------------------------------------------------

(setq                     ;; cmd window + src
  gdb-show-main t)

;;----------------------------------------------------------------------
;;                           working-copy
;;----------------------------------------------------------------------
(wc-enable-globally)

;; this is insanely great. It displays the function you are "in" in terms
;; of the point. Really nice for reading long functions.

(which-function-mode)

(defun pop-dired-in-source-file ()
  (interactive)
  (dired-other-window (file-name-directory buffer-file-name)) )

(defun configure-for-select ( select-inner select-outer )
  (local-set-key (kbd "C-c s w")  'select-word)

  (local-set-key (kbd "C-c s i")  select-inner)
  (local-set-key (kbd "C-c s o")  select-outer))

(defun configure-for-buffer-ring ( buffer-ring-mode )
  (when (buffer-ring-add buffer-ring-mode)
    (local-set-key (kbd "<M-right>")  'buffer-ring-next-buffer)
    (local-set-key (kbd "<M-left>")   'buffer-ring-prev-buffer)

    (local-set-key (kbd "<M-up>")   'buffer-torus-next-ring)
    (local-set-key (kbd "<M-down>") 'buffer-torus-prev-ring)) )

(grail-trap
  "Loading search paths."

  (load-user-elisp "search-paths"))

(defun configure-for-tags-uber ()
  (local-set-key (kbd "C-c i i") 'tags-uber-incremental)
  (local-set-key (kbd "C-c i s") 'tags-uber-search))

(defvar configure-programming-hook nil)

(defun configure-for-programming ( list-fn-signatures &optional buffer-ring-mode )
  "Enable my programming customizations for the buffer"

  (run-custom-hooks configure-programming-hook)

  ;; highlight all fucked up files.
  (whitespace-mode)

  (configure-for-tags-uber)

  (configure-for-buffer-ring buffer-ring-mode)

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (lang-repl-keybindings)

  ;; it is *really* handy to see just the function signatures of all the
  ;; functions defined in a buffer. It is so useful that every programming
  ;; mode needs to define a function so that it is bound to a key.
  (when list-fn-signatures
    (local-set-key (kbd "C-c s") list-fn-signatures))

  ;; for starters this will comment the region, but a toggle command needs
  ;; to be defined.
  (local-set-key (kbd "C-c u ;") 'uncomment-region)
  (local-set-key (kbd "C-c ;") 'comment-region)

  (local-set-key (kbd "C-c f s") 'sort-lines)

  ;; found this on emacs-wiki , all scripts are automatically made executable.
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t) )

(defun configure-for-navigation ( forwards backwards )
  (local-set-key (kbd "M-f") forwards)
  (local-set-key (kbd "M-b") backwards))

(defun configure-for-evaluation ( eval-define eval-expression eval-region eval-buffer )
  "Enable my programming customizations for the buffer

   eval-def     : this should evaluate definitions, variables,constants, and functions.
   eval-expr    : evaluate the current or last expression
   eval-region  : evaluate the region
   eval-buffer  : evaluate the buffer

   C-c e     will be the prefix for evaluation functions:

   C-c e d : evaluate a define
   C-c e e : evaluate last or current sexp
   C-c e r : evaluate the region.
   C-c e b : evaluate the buffer.
  "

  (let
    ((key-map (make-sparse-keymap)))

    (define-key key-map "d" eval-define)
    (define-key key-map "e" eval-expression)
    (define-key key-map "r" eval-region)
    (define-key key-map "b" eval-buffer)

    (define-key key-map "h" (keybindings-help-fn "evaluation" key-map))

    (local-set-key (kbd "C-c e") key-map)) )

(defun configure-for-docs ( docs-fn )
  (local-set-key (kbd "C-c h") docs-fn))

(defun configure-for-debugging ( eval-debug )
  (local-set-key (kbd "C-c d d") eval-debug))

(defun configure-for-macros ( expand-macro )
  (local-set-key (kbd "C-c m e") expand-macro) )

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
