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

(defun teardown-ediff-after-merge ()
  ;; cleanup ediff without asking or keeping the variants.
  (let
    ((merge ediff-buffer-C)
     (panel (current-buffer)))

    (ediff-janitor nil nil)

    (switch-to-buffer merge)
    (delete-other-windows)

    (kill-buffer panel) ))

(add-hook 'ediff-quit-hook 'teardown-ediff-after-merge t)

(provide 'merging)
