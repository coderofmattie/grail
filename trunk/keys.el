;;----------------------------------------------------------------------
;; keys.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;;
;; keybinding tools and configuration.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;;                    Global Key-Bindings
;;----------------------------------------------------------------------

;; enable ffap bindings so that C-x C-f on things like include directives
;; opens the paths. This could be very magical.

(ffap-bindings)

;; don't need compose mail right now, prefer maximize frame
(global-set-key (kbd "C-x m")   'maximize-frame)

;; this used to be minimize window, now it exits recursive editing
;; which is handy and safer.
(global-set-key (kbd "C-z")     'top-level)

;; reverse the regex/regular isearch

(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)

(global-set-key (kbd "M-C-s")   'isearch-forward)
(global-set-key (kbd "M-C-r")   'isearch-backward)

;; escape = (execute-extended-command)

;; standard emacs prompt for a interactive command

(global-set-key (kbd "<escape>") 'execute-extended-command)

;; C-xe   = (eval-expression)

;; evaluate the elisp expression given by the user

(global-set-key (kbd "C-x e")    'eval-expression)

;; M-TAB  = switch to the last buffer in the current window. cycles when
;;          repeated.

(global-set-key (kbd "<M-tab>")
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer)) ))

;; S-TAB  = Shift tab cycles between windows.

(global-set-key (kbd "<S-tab>") 'other-window)

(defun mattie-tab-switching ()
  (local-unset-key (kbd "<M-tab>"))
  (local-unset-key (kbd "<S-tab>")))



