;;----------------------------------------------------------------------
;; keybinding.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;;
;; keybinding tools and configuration.
;;----------------------------------------------------------------------

(require 'mattie-elisp)

(defun try-list (list)
  "iterate through the list of functions. If a function returns t for
   success terminate the iteration. It's a fancy or that assumes a list
   of functions."
  (catch 'terminate
    (dolist (func list)
      (if (funcall func)
        (throw 'terminate t)))
    nil
    ))

(defvar tab-context
  nil
  "a list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defun eval-tab-context ()
  "evaluate the tab context via the tab-context list of functions"
  (if tab-context
    (try-list tab-context)
    nil))

(defun contextualized-tab (completion-context)
  ;; generate a contextualized flavor of the tab key behavior.

  (lexical-let
    ((completion-function
       ;; TODO, handle a lambda coming in.
       ;; just a string selecting what is hardwired in here.

       (cond
         ((symbolp completion-context) completion-context)
         ;; use lisp-complete-symbol for elisp
         ((string-equal completion-context "elisp") 'lisp-complete-symbol)

         ;; fall back on dabbrev
         ((lambda () (dabbrev-expand nil)))
         )))

    ;; If I create a un-interned symbol I think my problems will go away.
    (make-anon-func "dwim-tab"
      (lambda ()
        "Complete if point is at end of a word, otherwise indent line."
        (interactive)

        ;; first try the tab context which should override the
        ;; general tab behavior only when the text or properties
        ;; essentially guarantee to DTRT
        (unless (eval-tab-context)
          (if (looking-at "\\>")
            ;; fall back on completion or indentation
            (funcall completion-function)
            (indent-for-tab-command)))
        ))
    ))

(defun bind-my-tab-keys ( completion-context )
  ;; when setting up major/minor modes adapt my global key-bindings to
  ;; the modes of the buffer. Local key-bindings that shadow my global
  ;; key-bindings are unset or replaced with contextualized variations
  ;; of my global defaults.

  (local-unset-key (kbd "<M-tab>"))
  (local-unset-key (kbd "<S-tab>"))

  (local-set-key (kbd "<tab>") (contextualized-tab completion-context))
  )

;;----------------------------------------------------------------------
;;                    Global Key-Bindings
;;----------------------------------------------------------------------

;; enable ffap bindings so that C-x C-f on things like include directives
;; opens the paths. This could be very magical.

(ffap-bindings)

;; don't need compose mail right now, prefer maximize frame
(global-set-key (kbd "C-x m") 'maximize-frame)

;; this used to be minimize window, now it exits recursive editing
;; which is handy and safer.
(global-set-key (kbd "C-z") 'top-level)

;; reverse the regex/regular isearch

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "M-C-s") 'isearch-forward)
(global-set-key (kbd "M-C-r") 'isearch-backward)

;; escape = (execute-extended-command)

;; standard emacs prompt for a interactive command

(global-set-key (kbd "<escape>") 'execute-extended-command)

;; C-xe   = (eval-expression)

;; evaluate the elisp expression given by the user

(global-set-key (kbd "C-x e") 'eval-expression)

;; M-TAB  = switch to the last buffer in the current window. cycles when
;;          repeated.

(global-set-key (kbd "<M-tab>")
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer))
    ))

;; S-TAB  = Shift tab cycles between windows.

(global-set-key (kbd "<S-tab>") 'other-window)

;;----------------------------------------------------------------------
;;                    Task specific Key-Bindings
;;----------------------------------------------------------------------

;; these setups should be considered inserts into other modes, they should
;; make no mode assumptions, and stay focused.

(defun bind-my-paren-keys ()
  "bind the parentheses to the brace keys, while the shifted
   paren keys become the braces."
  (interactive)

  ;; make the parentheses a bit easier to type, less shifting.
  (local-set-key (kbd "[") (lambda () (interactive) (insert-char ?\( 1 nil)))
  (local-set-key (kbd "]") (lambda () (interactive) (insert-char ?\) 1 nil)))

  (local-set-key (kbd "(") (lambda () (interactive) (insert-char ?\[ 1 nil)))
  (local-set-key (kbd ")") (lambda () (interactive) (insert-char ?\] 1 nil)))
  )
