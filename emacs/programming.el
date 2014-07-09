;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------
(require 'buffer-ring)

;;----------------------------------------------------------------------
;; indentation
;;----------------------------------------------------------------------

;; disable electric stuff to avoid problems with my more sophisticated
;; modes
(electric-indent-mode 0)

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

;; profiles not ready yet
(setq grail-masked-profiles (cons "clojure" grail-masked-profiles))

;; re-usable programming modules
(use-grail-profiles 0 "code-highlighting" "code-editing" "code-formatting" "repl")

;; higher level functionality
(use-grail-profiles 1 "lisp" "code-documentation" "version-control")

(use-grail-profiles 2 "emacs-lisp" "common-lisp" "sql" "scheme" "perl"
                      "shell-scripting" "web" "python" "clojure")

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

(defun toggle-comment-region ()
  "toggle-comment-region

   comment or uncomment the region
  "
  (interactive)
  (comment-or-uncomment-region (mark) (point)) )

(defun toggle-comment-buffer ()
  "toggle-comment-buffer

   comment or uncomment the region
  "
  (interactive)

  (mark-whole-buffer)
  (call-interactively 'toggle-comment-region) )

(defun configure-for-search-dummy ()
  (interactive)
  (message "function signature to find is not specified"))

(defvar configure-programming-hook nil)

(defun configure-for-programming ( list-fn-signatures &optional buffer-ring-mode )
  "Enable my programming customizations for the buffer"

  ;; whitespace
  (setq indent-tabs-mode nil)
  (whitespace-mode)

  ;; run hooks for programming configuration
  (run-custom-hooks configure-programming-hook)

  (configure-for-buffer-ring buffer-ring-mode)

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (let
    ((fn-search (or list-fn-signatures 'configure-for-search-dummy)))

    (custom-key-group "search" "s" nil
      ("f" . fn-search)
      ("g" . grep)
      ("r" . rgrep)
      ("o" . occur)
;;    ("i" . 'tags-uber-incremental)
;;    ("t" . 'tags-uber-search)
      ) )

  (custom-key-group "code editing" "c" nil
    ("c" . toggle-comment-region)
    ("b" . toggle-comment-buffer)
    ("i" . indent-region)
    ("s" . sort-lines) )

  ;; found this on emacs-wiki , all scripts are automatically made executable.
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t) )

(defun configure-for-navigation ( forwards backwards )
  (local-set-key (kbd "M-f") forwards)
  (local-set-key (kbd "M-b") backwards))

(defun configure-for-macros ( expand-macro )
  `(lambda ()
     (custom-key-group "macros" "m" nil
       ("e" . expand-macro)) ))

(defun configure-for-docs ( browse-docs )
  `(lambda ()
     (custom-key-group "macros" "i" nil
       ("b" . browse-docs)) ))

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
