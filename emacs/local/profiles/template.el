;;----------------------------------------------------------------------
;; template profile.
;;
;; support for the yasnippet template system with a generic interface
;; layered on. This glue API prevents the yasnippet API from creeping
;; into the rest of my config.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; load yassnippet template system
;;----------------------------------------------------------------------

(grail-load 'yasnippet (grail-define-installer "yasnippet"
                         "pkg"
                         'yasnippet))

;;----------------------------------------------------------------------
;; configure yassnippet
;;----------------------------------------------------------------------

;; This is a really ugly situation. The default non-nil behavior is to
;; unbind the yasnippet keymap and call the previous keymap function
;; which is of course dwim-tab. When calling yas/expand from inside
;; dwim-tab you get an infinite loop. setting this to nil fixes this.
(setq yas-fallback-behavior nil)

;; ignore filenames as triggers since the trigger name can be set in
;; the snippet without the restrictions imposed by the filesystem on
;; naming
(setq yas/ignore-filenames-as-triggers t)

(defconst yasnippet-local-templates
  (expand-file-name (concat grail-elisp-root "templates/yasnippet/"))
  "the yasnippet tree path relative to grail-elisp-root")

(defun templates/extend-templates ( extensions )
  "templates/configure-language LANGUAGE &REST MODES

   Setup templates for a language globaly. LANGUAGE is a
   string with the language name which should be a path
   relative to TEMPLATES-ROOT. the modes is a series of
   strings such as \"emacs-lisp-mode\". A function to
   configure the mode/buffer will be added to the
   entry point hook for the mode.
  "
  (let
    (make-variable-buffer-local 'yas-snippet-dirs)

    ;; add it to the list of paths kept by yasnippet
    (when (file-accessible-directory-p extensions)
      (setq yas-snippet-dirs (cons extensions yas-snippet-dirs))
      (yas-load-directory extensions) ) ))


(templates/extend-templates yasnippet-local-templates)
(yas-reload-all)

(defun templates/mode-setup ()
  ;; activate yasnippet in the buffer
  (yas-minor-mode)

  (dwim-tab-localize-context 'template/expand)
  (dwim-tab-localize-context 'template/next)

  (local-set-key (kbd "C-c t l") 'templates/list)

  (local-set-key (kbd "C-c t e") 'template/expand)
  (local-set-key (kbd "C-c t n") 'template/next)

  (local-set-key (kbd "C-c t c") 'template/new))

(defun templates/list ()
  "templates/list

   List the current template table for the buffer's mode.
  "
  (interactive)
  (yas-describe-tables))

;;----------------------------------------------------------------------
;; template commands.
;;----------------------------------------------------------------------

(defun template/in-field-p ()
  "template/in-field-p

   return true when the point is in a yasnippet field
  "
  (when (mode-overlay-at-point-p 'yas--field) t))

(defun template/expand ()
  "template/expand

   assuming the point is at a template name expand the template.
  "
  (interactive)
  (yas-expand))

(defun template/next ()
  "template/next

   move to the next field in the template.
  "
  (interactive)
  (if (template/in-field-p)
    (progn
      (yas-next-field)
      t)
    nil))

(defun template/new ()
  "templates/new

   create a new template.
  "
  (interactive)
  (yas-new-snippet))

;;----------------------------------------------------------------------
;; configure the various languages for template support.
;;----------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (templates/mode-setup))
  t)

(add-hook 'cperl-mode-hook
  (lambda ()
    (templates/mode-setup))
  t)
