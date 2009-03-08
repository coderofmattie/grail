;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; template expansion
;;----------------------------------------------------------------------
(require 'xml-code)

(use-grail-groups "template" "lisp" "code-formatting")

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

  ;; use Ctrl-l as the prefix for e commands. It's short
  ;; and the usual unix meaning of centering a screen is
  ;; a small loss.
  (local-unset-key (kbd "C-l"))

  (local-set-key (kbd "M-f") 'forward-sexp)
  (local-set-key (kbd "M-b") 'backward-sexp)

;;  (when (xml-code-templates-p)
;;    (local-set-key (kbd "C-l n") 'else-next-placeholder)
;:    (local-set-key (kbd "C-l k") 'else-kill-placeholder)

;;    (local-set-key (kbd "C-l l") 'xml-code-kill-template))
  )

;;----------------------------------------------------------------------
;; elisp
;;----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'configure-for-programming)

;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------
(grail-activate-with-recovery "programming.el" quack
  (("quack" . "http://www.neilvandyke.org/quack/quack.el"))

  (setq
    auto-mode-alist (append '(("\\.scheme$"    . quack-mode)
                              ("\\.scm$"       . quack-mode)
                               ) auto-mode-alist ))
  )

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
      cperl-invalid-face (quote off)   ;; disable trailing whitespace highlighting with _
      cperl-pod-here-scan nil          ;; more attempts to speed up font-lock

      cperl-indent-parens-as-block t   ;; This was a critical fix , no more
                                       ;; data structure indenting to the opening brace

      cperl-indent-level 2             ;; indentation adjustments
      cperl-continued-statement-offset 2)

    ;; (grail-set-faces
    ;;   (cperl-array-face (slant 'italic))
    ;;   (cperl-hash-face  (slant 'italic))
    ;;   (cperl-nonoverridable-face (foreground "DeepSkyBlue4"))
    ;;   )

    (add-hook 'cperl-mode-hook
      (lambda ()
        (xml-code-for-language "perl5")
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

;; what is the cc-mode hook order, are the language or the common hooks run first ?
(eval-after-load "cc-mode"
  (progn
    (add-hook 'c-mode-common-hook
      (lambda ()
;;      (c-set-style "linux")          ;; base off of linux style

        (setq c-basic-offset 2)               ;; tabs are 2 spaces
        (c-set-offset 'substatement-open '0)  ;; hanging braces

        ;; auto-hungry newline and whitespace delete
        (c-toggle-auto-hungry-state 1) ))

    (add-hook 'c-mode-hook
      (lambda ()
        (xml-code-for-language "c")
        (configure-for-programming)))


    (add-hook 'c++-mode-hook
      (lambda ()
        (xml-code-for-language "c++")
        (configure-for-programming))) ))

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
