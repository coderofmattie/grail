;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------

;; stateful version by fledermous in #emacs (thanks)
;; (remove nil (mapcar fun (remove nil list)))

;; stateful version by sabetts in #emacs (thanks).
;;(defun map-reduce (fn &rest list)
;;  (let (acc v)
;;    (while list
;;      (setq v (pop list)
;;            v (and v (funcall v)))
;;      (when v (push v acc)))
;;    acc))


(defun instrument-function-at-call ()
  "instrument the function indicated by the call at the point. Warning, will eval the defun."
  (interactive)
  (save-excursion
    (find-function)
    (edebug-defun)))

;;----------------------------------------------------------------------
;;          tags source code indexing
;;----------------------------------------------------------------------

;; this may be obsolete with how cedet does a database of multiple files.

(obsoloted 'gtags)
(defun tune-gtags ()
  (gtags-mode)

  ;; bind the global keys, f for find , r for references , and p for pop.

  (local-set-key "\C-c/f" 'gtags-find-tag)
  (local-set-key "\C-c/r" 'gtags-find-rtag)
  (local-set-key "\C-c/p" 'gtags-pop-stack)
  )

;; FROM tune-programming
(tune-gtags)

;;----------------------------------------------------------------------
;; Map modes to file formats
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '( ("\\.xsl$"     . xsl-mode)
				 ("\\.scheme$"  . scheme-mode)
				 ) auto-mode-alist ))

;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------
(setq inferior-lisp-program "/usr/bin/clisp")
;; (require 'slime)
;; (slime-setup)


;; FROM: tune-programming

;; (require 'paredit)

;;  (paredit-mode +1)

;;  (define-key paredit-mode-map (kbd "(") 'paredit-open-parenthesis)
;;  (define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis)

;;  (local-set-key "\C-c/r" 'query-replace-regexp)


;;----------------------------------------------------------------------
;; old experiments that worked but did not pan out.
;;----------------------------------------------------------------------

;; (defun my-test ()
;;  "foo"
;;  (interactive)
;;  (if (skip-over-properties (next "\(\)") (face ".*comment.*" ".*string.*" ".*doc.*"))
;;    (message "it worked !")
;;    (message "it failed !")
;;    ))

(defun lisp-list-delete-body ()
  "delete the body of a lisp list including any nested lists"
  (interactive)
  (let
    ((open-pos (scan-lisp-list-open))
     (close-pos (scan-lisp-list-close)))

    (delete-backward-char (- close-pos open-pos))))

(defun paludis-overlay ()
  (auto-overlay-unload-regexp 'paludis)
  (auto-overlay-load-regexp
    `(word ("^\\\*[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]*$" . 1)
       (face . deploy-package-name-face)
       (paludis-type . name)
       (local-map . ,paludis-keymap)
       (read-only . t) ;; still doesn't work
       )
    'paludis
    )

  (auto-overlay-load-regexp
    `(word ("^[[:blank:]]+\\([^[:blank:]]+:\\)[[:blank:]]*" . 1)
       (face . deploy-repository-face)
       (paludis-type . name)
       (local-map . ,paludis-keymap)
       (read-only . t) ;; still doesn't work
       )
    'paludis
    )

  (auto-overlay-start 'paludis)
  )

