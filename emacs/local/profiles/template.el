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
                         "git"
                         "https://github.com/capitaomorte/yasnippet.git"))

;;----------------------------------------------------------------------
;; configure yassnippet
;;----------------------------------------------------------------------

;; This is a really ugly situation. The default non-nil behavior is to
;; unbind the yasnippet keymap and call the previous keymap function
;; which is of course dwim-tab. When calling yas/expand from inside
;; dwim-tab you get an infinite loop. setting this to nil fixes this.

;; this also fixes expand going into a template selection menu which
;; is not what is wanted since I want to seperate expansion from
;; completion using dwim-tab
(setq yas-fallback-behavior nil)

;; ignore filenames as triggers since the trigger name can be set in
;; the snippet without the restrictions imposed by the filesystem on
;; naming
(setq yas/ignore-filenames-as-triggers t)

;; this seems to be necessary now that yas clobbering of tab
;; cannot be turned off
(strip-minor-mode-keymap 'yas-minor-mode)

(defconst yasnippet-local-templates
  (expand-file-name (concat grail-elisp-root "templates/yasnippet/"))
  "the yasnippet tree path relative to grail-elisp-root")

(defconst yasnippet-collections
  '(("rejeep" "https://github.com/rejeep/yasnippets.git")
    ("local" nil)))

(defun templates-update-collections ()
  (let
    ((new-dirs nil))

    (mapc
      (lambda ( dir )
        (when (file-accessible-directory-p dir)
          (setq new-dirs (cons dir new-dirs))) )
      yas-snippet-dirs)

    (mapc
      (lambda ( collection )
        (let*
          ((collection-name (car collection))
           (collection-dir (concat yasnippet-local-templates "/" collection-name "/")))

          (if (file-directory-p collection-dir)
            (setq new-dirs (cons collection-dir new-dirs))
            (let
              ((retrieve (cadr collection)))

              (if retrieve
                (progn
                  (grail-git-templates yasnippet-local-templates collection-name retrieve)
                  (setq new-dirs (cons collection-dir new-dirs))
                  (message "templates: installed collection %s" collection-name))
                (message "templates: no way to install missing collection %s" collection-name)) )) ))
      yasnippet-collections)

    (setq yas-snippet-dirs new-dirs)

    (yas-reload-all) ))

(templates-update-collections)

;; strip off all the completion methods so that
;; only my completion system is used.
(setq yas-prompt-functions nil)

;;----------------------------------------------------------------------
;; template dwim-tab functions
;;----------------------------------------------------------------------

(defun template/in-field-p ()
  "template/in-field-p

   return true when the point is in a yasnippet field
  "
  (mode-overlay-near-point-p 'yas--field))

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

;;----------------------------------------------------------------------
;; template commands
;;----------------------------------------------------------------------

(defun templates/list ()
  "templates/list

   List the current template table for the buffer's mode.
  "
  (interactive)
  (yas-describe-tables))


(defun template/new ()
  "templates/new

   create a new template.
  "
  (interactive)
  (yas-new-snippet))

(defun template/insert ()
  "templates/snippet

   insert a snippet.
  "
  (interactive)
  (yas-insert-snippet))

;;----------------------------------------------------------------------
;; dwim-complete integration
;;----------------------------------------------------------------------

(defun templates/yas-all-template-names ()
  (yas--table-all-keys (car (yas--get-snippet-tables))) )

(defun templates/yas-get-template-pair-by-name ( name )
  (let
    (( templates nil ))

    (mapc
      (lambda ( table )
        (mapc
          (lambda ( template-pair )
            (when (string-equal (car template-pair) name)
              (setq templates (cons template-pair templates)) ))
          (yas--table-templates table)) )

      (yas--get-snippet-tables))

    templates))

(defun templates/yas-select-from-multiple-templates ( template-list )
  (message "templates: warning multiple templates")
  (car template-list))

(defun templates/yas-expand-template-from-pair ( template-pair )
  (yas-expand-snippet
    (yas--template-content ( cdr template-pair ))) )

(defun templates/yas-action ( selection )
  (let
    ((template (templates/yas-select-from-multiple-templates
                 (templates/yas-get-template-pair-by-name (format "%s" selection))) ))

    (if template
      (progn
        (dwim-complete-delete-stem)
        (templates/yas-expand-template-from-pair template))
      (message "template/action: no template for %s" selection)) ))

(defun templates/yas-source ()
  (dwim-complete/make-source "templates"
    (templates/yas-all-template-names)
    (dwim-complete/make-action 'templates/yas-action)) )

;;----------------------------------------------------------------------
;; mode setup function
;;----------------------------------------------------------------------

(defun ver-ctl-log-bindings ()
  (let
    ((ver-map (make-sparse-keymap)))

    (define-key ver-map "l" 'ver-ctl-log-file-list)
    (define-key ver-map "c" 'ver-ctl-log-file-create)
    (define-key ver-map "o" 'ver-ctl-log-file-open)
    (define-key ver-map "i" 'ver-ctl-log-insert-label)
    (define-key ver-map "m" 'ver-ctl-log-insert-log)

    (define-key ver-map "h" (keybindings-help-fn "ver log" ver-map))
    (local-set-key (kbd "C-c l") ver-map)))

(defun templates/mode-setup ()
  ;; activate yasnippet in the buffer
  (yas-minor-mode)

  (dwim-tab-localize-context 'template/expand)
  (dwim-tab-localize-context 'template/next)

  (let
    ((key-map (make-sparse-keymap)))

    (define-key key-map "l" 'templates/list)

    (define-key key-map "i" 'template/insert)
    (define-key key-map "e" 'template/expand)
    (define-key key-map "n" 'template/next)

    (define-key key-map "c" 'template/new)

    (define-key key-map "h" (keybindings-help-fn "templates" key-map))

    (local-set-key (kbd "C-c t") key-map))

  (dwim-complete-local-add-source 'templates/yas-source))

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

(add-hook 'web-mode-hook
  (lambda ()
    (templates/mode-setup))
  t)

(add-hook 'log-mode-hook
  (lambda ()
    (templates/mode-setup))
  t)

(add-hook 'sh-mode-hook
  (lambda ()
    (templates/mode-setup))
  t)
