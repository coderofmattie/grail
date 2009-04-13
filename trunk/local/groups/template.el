;;----------------------------------------------------------------------
;; template group
;;----------------------------------------------------------------------

(defvar template-group-yasnippet-installer
  (grail-define-installer "yasnippet" "tar:bz2"
    "http://yasnippet.googlecode.com/files/yasnippet-0.5.10.tar.bz2") "the yasnippet installer")

(grail-activate-with-recovery "template" yasnippet template-group-yasnippet-installer
  ;; don't automatically activate.
  (setq yas/dont-activate t)

  (defun in-yasnippet-reference ()
    (interactive)
    (when (mode-overlay-at-point-p 'yas/snippet-reference) t))

  (defun dwim-tab-yas-next-field ()
    (when (in-yasnippet-reference)
      (yas/next-field-group)
      t))

  (defun dwim-tab-yas-expand ()
    (interactive)
    (when (equal 'expanded (yas/expand)) t))
  )

;; for each mode we can do a load ....
;;'yas/minor-mode-auto-on
;;  (yas/load-directory (grail-garuntee-dir-path (grail-sanitize-path (concat grail-elisp-root "templates/yasnippet"))))





