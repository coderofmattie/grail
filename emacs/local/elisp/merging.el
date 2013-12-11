;;----------------------------------------------------------------------
;; merging
;;----------------------------------------------------------------------
(require 'diff)
(require 'external-merge-command)

(setq
  diff-switches "-U3")                 ;; turn on standard context diffs,

(add-hook 'diff-mode-hook

  ;; when diff is called it will pop a window which is nice, but killing
  ;; the buffer did not get rid of the popped window , until now.

  (lambda ()
    (add-hook 'kill-buffer-hook 'rid-window t t))
  t)

;;----------------------------------------------------------------------
;;                          Ediff
;;----------------------------------------------------------------------

;; I will enventually create my interactive merge interface with this
;; mode as a foundation. Until then tweak the basics.

(require 'ediff)          ;; 2-3 way merge tool, can be used
                          ;; for cherry picking and splitting

(setq
  ediff-custom-diff-options "-U3"         ;; same for ediff

  ediff-split-window-function 'split-window-horizontally
  ediff-merge-split-window-function 'split-window-horizontally

  ediff-window-setup-function 'ediff-setup-windows-plain

  ediff-auto-refine t)

(defun merging-buffers-to-name ()
  (mapcar
    (lambda ( buffer-object )
      (buffer-name buffer-object))
    (buffer-list)) )

(defun merging-buffer-predicate-p ( buffer-name buffer-spec )
  (let*
    (( case-fold-search t )
     ( result (string-match buffer-spec buffer-name) ))

    (if (eq nil result)
      nil
      t)))

(defun merging-matching-buffers-by-name ( match-spec )
  (let
    (( match-list nil ))

    (mapc
      (lambda ( buffer-name )
        (when (eval `(or
                       ,@(mapcar
                           (lambda ( spec )
                             (merging-buffer-predicate-p buffer-name spec))
                           match-spec)))

          (setq match-list (cons buffer-name match-list)) ))
        (merging-buffers-to-name))
    match-list))

(defun merging-kill-all-listed ( kill-list )
  (mapc
    (lambda ( buffer-name )
      (kill-buffer buffer-name))
    kill-list) )

(defconst merging-ediff-interface-buffer-regex-list '( "\*.*Ediff.*\*"  ))

(defun merging-ediff-teardown-interface ( keep-buffer )
  (switch-to-buffer keep-buffer)
  (delete-other-windows)

  (merging-kill-all-listed
    (merging-matching-buffers-by-name merging-ediff-interface-buffer-regex-list)) )

(defvar merging-ediff-teardown-diff-egg-toggle nil)

(defun merging-ediff-teardown-diff-egg-toggle-enable ()
  (setq merging-ediff-teardown-diff-egg-toggle t))

(defun merging-ediff-teardown-diff-egg-toggle-disable ()
  (setq merging-ediff-teardown-diff-egg-toggle nil))

(defun merging-ediff-teardown-diff-egg ()
  (kill-buffer ediff-buffer-A)
  (merging-ediff-teardown-interface ediff-buffer-B)

  (merging-ediff-teardown-diff-egg-toggle-disable))

(add-hook 'ediff-quit-hook 'merging-ediff-teardown-diff-egg t)

(remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)

(provide 'merging)
