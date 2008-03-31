;;----------------------------------------------------------------------
;; linux.el
;;----------------------------------------------------------------------

;; Gentoo has a file that tunes emacs and loads the third party
;; components managed by the package manager.

(load-file "/usr/share/emacs/site-lisp/site-gentoo.el")

;;----------------------------------------------------------------------
;; sawfish.
;;----------------------------------------------------------------------
(eval-after-load "sawfish"
  (add-hook 'sawfish-mode-hook
    (lambda ()
    ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
    ;; this function/keybinding should be used exclusively to avoid frustrating
    ;; errors.
      (local-set-key (kbd "C-x e") 'sawfish-eval-defun)

      (dwim-tab-localize 'sawfish-complete-symbol)

      ;; bind-my-paren-keys is not required, because sawfish runs the elisp hooks
      ;; evidently. just need to re-setup my tab key.
      )))


