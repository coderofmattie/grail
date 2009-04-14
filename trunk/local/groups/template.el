;;----------------------------------------------------------------------
;; template group
;;----------------------------------------------------------------------

(defvar template-group-yasnippet-installer
  (grail-define-installer "yasnippet" "tar:bz2"
    "http://yasnippet.googlecode.com/files/yasnippet-0.5.10.tar.bz2") "the yasnippet installer")

(defvar templates-enabled nil)
(defvar templates-loaded nil "a list of the languages with templates loaded")

(defun templates-enabled-p ()
  "templates-enabled-p

   return non-nil when templates are enabled.
  "
  (when templates-enabled t))

(grail-activate-with-recovery "template" yasnippet template-group-yasnippet-installer
  ;; don't automatically activate.
  (setq yas/dont-activate t)

  (defvar template-group-yasnippet-tree
    (expand-file-name (concat grail-elisp-root "templates/yasnippet/"))
    "the yasnippet tree path relative to grail-elisp-root")

  ;; these have the generic template reference so I can switch or mix
  ;; template systems without going on a grep marathon.

  (defun template-in-reference ()
    (when (mode-overlay-at-point-p 'yas/snippet-reference) t))

  (defun template-next-next-field ()
    (interactive)
    (when (template-in-reference)
      (yas/next-field-group)
      t))

  (defun templates-load-for-language ( language )
    (let
      ((templates (concat template-group-yasnippet-tree language)))

      (when (file-accessible-directory-p templates)
        (yas/load-directory templates)
        (add-to-list 'templates-loaded language)) ))

  (defun template-expand ()
    (interactive)
    (when (equal 'expanded (yas/expand)) (throw 'terminate-complete t))
    nil)

  ;; signal that a template system is activated
  (setq templates-enabled t)

  ;; always load the lisp templates.
  (templates-load-for-language "lisp")

  (eval-after-load 'cperl-mode
    '(templates-load-for-language "perl")))





