;;----------------------------------------------------------------------
;; slime
;;
;; slime profile for common lisp coding
;;----------------------------------------------------------------------
(require 'custom-key)
(require 'borg-repl)

(require 'profile/common-lisp)

(defconst cl-repl-name (borg-repl/repl-name cl-lisp-name))

(grail-load-package 'slime "git" "https://github.com/slime/slime")

(setq hyperspec-dir nil)

;; (setq hyperspec-dir
;;   (grail-fetch-docs "hyperspec"
;;     (grail-define-installer "hyperspec" "tar:gz"
;;       "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
;;     1))

;;
;; basic setup
;;

(setq
  slime-net-coding-system 'utf-8-unix

  inferior-lisp-program "sbcl"

  slime-words-of-encouragement '("The name is Bond. James Bond."
                                  "These are your father's parentheses. Elegant weapons from a more civilized age."
                                  "We were on the edge of the desert when the Emacs took hold."
                                  "Mine says: Desert Eagle ... .50")
  common-lisp-hyperspec-root (concat "file://" hyperspec-dir "/"))

;; (setq common-lisp-hyperspec-default (concat common-lisp-hyperspec-root "Body/00_.htm"))
;; (code-documentation-setup "lisp-mode-docs" "lisp-mode" common-lisp-hyperspec-default)

;;
;; dwim setup
;;

(defun profile/slime-candidates ()
  (car (slime-simple-completions "")))

(defun profile/slime-source ()
  (dwim-complete/make-source "slime"
    'profile/slime-candidates
    'dwim-complete-replace-stem ))

(defun profile/slime-dwim-setup ()
  (unless (dwim-complete-mode-check-type cl-lisp-name "mode")
    (dwim-complete-mode-add-source cl-lisp-name (profile/slime-source))
    (dwim-complete-mode-add-type cl-lisp-name "mode"))

  (dwim-complete/for-buffer) )

;;
;; repl buffer setup
;;

(defun profile/slime-repl-setup ()
  (grail-require profile/syntax-tools "emacs-lisp" "syntax"
    (profile/syntax-tools-mode-setup)
    (profile/syntax-tools-lisp) )

  (buffer-ring/add cl-repl-name)
  (buffer-ring/local-keybindings)

  (profile/slime-dwim-setup) )

(add-hook 'slime-connected-hook 'profile/slime-repl-setup t)

;;
;; borg repl functions
;;

(defun profile/slime-repl-create ()
  "profile/slime-repl-create

   create a new slime REPL.
  "
  (interactive)
  (slime) )

(defun profile/slime-lisp-setup ()

  ;; turn on minor mode
  (slime-mode t)

  (dwim-complete/set-mode cl-lisp-name)
  (profile/slime-dwim-setup)

  (dwim-tab-localize-context (dwim-tab-make-expander 'dwim-tab-stem-trigger 'slime-complete-symbol))

  (borg-repl/bind-repl cl-repl-name
    'profile/slime-repl-create
    'slime-eval-last-expression
    'slime-eval-region
    'slime-eval-buffer
    'slime-eval-defun)

  (borg-repl/bind-macro-expand 'slime-macroexpand-1) )

(add-hook 'lisp-mode-hook 'profile/slime-lisp-setup t)

(provide 'grail/slime)

