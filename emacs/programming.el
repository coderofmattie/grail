;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------
(require 'merging)
(require 'ext-merging)

(require 'buffer-ring)

(require 'programming-generic)

;;----------------------------------------------------------------------
;; indentation
;;----------------------------------------------------------------------

;; disable electric stuff to avoid problems with my more sophisticated
;; modes

(electric-indent-mode 0)

;;----------------------------------------------------------------------
;; programming packages not dependent on third party support
;;----------------------------------------------------------------------

;; profiles not ready yet
(mask-grail-profiles "clojure")

;; re-usable programming modules
(use-grail-profiles 0 "code-highlighting" "syntax-tools" "code-formatting")

;; higher level functionality
(use-grail-profiles 1 "version-control")

(use-grail-profiles 2 "emacs-lisp" "common-lisp" "sql" "scheme" "perl5"
                      "shell-scripting" "web" "python" "clojure")

;; advanced functionality
(use-grail-profiles 3 "template" "slime")

;;----------------------------------------------------------------------
;;                          misc tools
;;----------------------------------------------------------------------

(setq                     ;; cmd window + src
  gdb-show-main t)

(which-function-mode)

;;----------------------------------------------------------------------
;; C/C++ common
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.c$"       . c-mode)
                                ("\\.cc$"      . c++-mode)
                                ("\\.cpp$"     . c++-mode)
                                ("\\.h$"       . c++-mode)
                                 ) auto-mode-alist ))

(defun c-mode-generic-setup ()
  (c-set-style "linux")                 ;; base off of linux style
  (setq c-basic-offset 2)               ;; tabs are 2 spaces

  (c-set-offset 'substatement-open '0)  ;; hanging braces

  ;; auto-hungry newline and whitespace delete
  (c-toggle-auto-hungry-state 1) )

(add-hook 'c-mode-common-hook 'c-mode-generic-setup t)

(defconst c-mode-name "C")

(defun c-mode-setup ()
  (programming-mode-generic)

  (buffer-ring/add c-mode-name)
  (buffer-ring/local-keybindings) )

(add-hook 'c-mode-hook 'c-mode-setup t)

(defconst c-mode-name "C++")

(defun c++-mode-setup ()
  (programming-mode-generic)

  (buffer-ring/add c++-mode-name)
  (buffer-ring/local-keybindings) )

(add-hook 'c++-mode-hook 'c++mode-setup t)


