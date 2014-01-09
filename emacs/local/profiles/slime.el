;;----------------------------------------------------------------------
;; slime
;;
;; slime profile for common lisp coding
;;----------------------------------------------------------------------
(require 'lang-repl)

(grail-load 'slime     (grail-define-installer "slime"
                         "cvs"
                         ":pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot"))

(setq hyperspec-dir
  (grail-fetch-docs "hyperspec"
    (grail-define-installer "hyperspec" "tar:gz"
      "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
    1))

;;----------------------------------------------------------------------
;; SLIME
;;----------------------------------------------------------------------
(require 'slime)

(setq
  slime-net-coding-system 'utf-8-unix

  inferior-lisp-program "sbcl"

  slime-words-of-encouragement '("The name is Bond. James Bond."
                                  "These are your father's parentheses. Elegant weapons from a more civilized age."
                                  "We were on the edge of the desert when the Emacs took hold."
                                  "Mine says: Desert Eagle ... .50")
  common-lisp-hyperspec-root (concat "file://" hyperspec-dir "/"))

(setq common-lisp-hyperspec-default (concat common-lisp-hyperspec-root "Body/00_.htm"))

;;----------------------------------------------------------------------
;; mode setup
;;----------------------------------------------------------------------

(defun dwim-complete/slime-candidates ()
  (car (slime-simple-completions "")))

(defun dwim-complete/slime-source ()
  (dwim-complete/make-source "slime"
    (dwim-complete/slime-candidates)
    (dwim-complete/make-action 'dwim-complete-replace-stem) ))

(add-hook 'slime-connected-hook
  (lambda ()
    (lisp-smart-parens-editing)

    (configure-for-buffer-ring "lisp-mode")

    (code-documentation-setup "lisp-mode-docs" "lisp-mode" common-lisp-hyperspec-default)

    (unless (dwim-complete-mode-check-type "slime-mode" "mode")
      (dwim-complete-mode-add-source "slime-mode" (dwim-complete/slime-source))
      (dwim-complete-mode-add-type "slime-mode" "mode"))

    (dwim-complete/for-buffer)

    (lang-repl-mode-add "lisp-mode" (buffer-name))
    (add-hook 'kill-buffer-hook (lang-repl-mode-del-hook-fn "lisp-mode") t t))
  t)

(defun lang-repl/slime ( first )
  (slime))

(lang-repl-mode-define "lisp-mode" 'lang-repl/slime)

(add-hook 'lisp-mode-hook
  (lambda ()
    (slime-mode t)

    (unless (dwim-complete-mode-check-type major-mode "mode")
      (dwim-complete-mode-add-source major-mode (dwim-complete/slime-source))
      (dwim-complete-mode-add-type major-mode "mode"))

    (code-documentation-setup "lisp-mode-docs" "lisp-mode" common-lisp-hyperspec-default)
    (dwim-tab-localize-context 'slime-complete-symbol)

    (configure-for-evaluation
      'slime-eval-defun
      'slime-eval-last-expression
      'slime-eval-region
      'slime-eval-buffer) )
  t)
