;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------
(require 'buffer-ring)
(require 'custom-key)

(require 'buffer-status)

;;----------------------------------------------------------------------
;; programming packages not dependent on third party support
;;----------------------------------------------------------------------

(require 'tags-uber)
(require 'working-copy)

(require 'merging)
(require 'ext-merging)

(require 'lang-repl)

;;----------------------------------------------------------------------
;;                           working-copy
;;----------------------------------------------------------------------
(wc-enable-globally)

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

  ;; buffer-ring
  (buffer-ring/local-keybindings)
  (buffer-ring/add buffer-ring-mode)

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (let
    ((fn-search (or list-fn-signatures 'configure-for-search-dummy)))

    (custom-key-group "search" "s" nil
      ("f" . fn-search)
      ("g" . grep)
      ("r" . rgrep)
      ("o" . occur)
;;    ("i" . tags-uber-incremental)
;;    ("t" . tags-uber-search)
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

(provide 'user-programming)
