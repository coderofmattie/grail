;;----------------------------------------------------------------------
;; notes.el
;;----------------------------------------------------------------------

(grail-load 'org (grail-define-installer "org-mode"
                  "git"
                   "git://orgmode.org/org-mode.git"))

(defun notes-profile/bindings ()
  (let
    ((notes-map (make-sparse-keymap)))

    (define-key notes-map "t" 'outline-toggle-children)
    (define-key notes-map "g" 'org-shifttab)

    (define-key notes-map "n" 'outline-next-visible-heading)
    (define-key notes-map "p" 'outline-previous-visible-heading)
    (define-key notes-map "u" 'outline-up-heading)

    (define-key notes-map "l" 'org-insert-link)

    (define-key notes-map "h" (keybindings-help-fn "notes" notes-map))

    (local-set-key (kbd "C-c o") notes-map)))

(defun notes-profile/setup ()
  (interactive)
  (notes-profile/bindings)
  (turn-on-dwim-tab)

  (make-variable-buffer-local 'org-tab-first-hook)
  (add-to-list 'org-tab-first-hook 'dwim-tab-do-magic)

  (flyspell-mode) )

(add-hook 'org-mode-hook 'notes-profile/setup)
