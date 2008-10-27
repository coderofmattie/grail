;;----------------------------------------------------------------------
;; programming.el
;;----------------------------------------------------------------------

(use-styles "template" "lisp" "code-formatting")

;;----------------------------------------------------------------------
;;                       misc.
;;----------------------------------------------------------------------

;; found this on emacs-wiki , all scripts are automatically made executable.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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

  ediff-use-toolbar-p nil              ;; doesnt work ? disable the toolbar in ediff
  ediff-window-setup-function 'ediff-setup-windows-plain ;; this should work.
  )

;;----------------------------------------------------------------------
;; enhanced merging
;;----------------------------------------------------------------------
(require 'merc)

;;----------------------------------------------------------------------
;; template expansion
;;----------------------------------------------------------------------
(require 'else-xml)

(require 'yasnippet)


;; this is insanely great. It displays the function you are "in" in terms
;; of the point. Really nice for reading long functions.

(which-function-mode)

;; some mundane asthetics and keybindings plus whatever dwim input
;; expansion I can cook up.

(defun configure-for-programming ()
  "Enable my programming customizations for the buffer"

  (turn-on-font-lock)                     ;; enable syntax highlighting

  ;; (turn-on-filladapt-mode)             ;; smart comment line wrapping

  (mattie-tab-switching)                  ;; my personal tab key setup.

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  ;; create a default register that shortens repeated
  ;; register commands
  (set (make-local-variable 'default-register) 'a)

  ;; use Ctrl-l as the prefix for e commands. It's short
  ;; and the usual unix meaning of centering a screen is
  ;; a small loss.
  (local-unset-key (kbd "C-l"))

  (local-set-key (kbd "M-f") 'forward-sexp)
  (local-set-key (kbd "M-b") 'backward-sexp)

  (local-set-key (kbd "C-l s") 'assign-default-register)
  (local-set-key (kbd "C-l r") 'list-registers)

  (local-set-key (kbd "C-l w") 'set-default-register)
  (local-set-key (kbd "C-l i") 'insert-default-register)

  (when (else-xml)
    ;; here is where C-xe will expand templates
    (local-set-key (kbd "C-l e") 'else-expand-placeholder)
    (local-set-key (kbd "C-l n") 'else-next-placeholder)

    (local-set-key (kbd "C-l k") 'else-kill-placeholder)

    (local-set-key (kbd "C-l l") 'else-show-token-names) ))

;;----------------------------------------------------------------------
;; elisp
;;----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'configure-for-programming)

;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------
(defalias 'perl-mode 'cperl-mode)

(setq
  auto-mode-alist (append '(("\\.pl$"      . cperl-mode)
                            ("\\.pm$"      . cperl-mode)
                             ) auto-mode-alist ))

(eval-after-load "cperl-mode"
  (progn
    (setq
      else-mode-xml-alist (cons '("perl5" . ("perl5.xml"
                                             "loop.xml"))
                            else-mode-xml-alist)

      cperl-invalid-face (quote off)   ;; disable trailing whitespace highlighting with _
      cperl-pod-here-scan nil          ;; more attempts to speed up font-lock

      cperl-indent-parens-as-block t   ;; This was a critical fix , no more
                                       ;; data structure indenting to the opening brace

      cperl-indent-level 2             ;; indentation adjustments
      cperl-continued-statement-offset 2)

    (add-hook 'cperl-mode-hook
      (lambda ()
        (set (make-local-variable 'source-language) "perl5")
        (configure-for-programming)
        (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point))) ))

;;----------------------------------------------------------------------
;; C/C++ common
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.c$"       . c-mode)
				("\\.cc$"      . c++-mode)
				("\\.cpp$"     . c++-mode)
				("\\.h$"       . c++-mode)
                                 ) auto-mode-alist ))

(eval-after-load "cc-mode"
  (add-hook 'c-mode-common-hook
    (lambda ()
      (configure-for-programming)

;;      (c-set-style "linux")          ;; base off of linux style

      (setq c-basic-offset 2)               ;; tabs are 2 spaces
      (c-set-offset 'substatement-open '0)  ;; hanging braces

      (c-toggle-auto-hungry-state 1) ;; auto-hungry newline and
                                     ;; whitespace delete
      )) )

;;----------------------------------------------------------------------
;; Java
;;----------------------------------------------------------------------
(eval-after-load "java-mode"
  (require 'flymake) )

;;----------------------------------------------------------------------
;; Lua
;;----------------------------------------------------------------------

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist ))

(eval-after-load "lua-mode"
  (add-hook 'lua-mode-hook
    (lambda ()
      (configure-for-programming)
      )) )
