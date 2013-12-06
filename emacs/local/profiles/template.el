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
(setq yas-fallback-behavior nil)

;; ignore filenames as triggers since the trigger name can be set in
;; the snippet without the restrictions imposed by the filesystem on
;; naming
(setq yas/ignore-filenames-as-triggers t)

(setq yas-trigger-key nil)

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

(setq yas-prompt-functions
  '(template/helm-prompt
    yas-ido-prompt))

(defun templates/mode-setup ()
  ;; activate yasnippet in the buffer
  (yas-minor-mode)

  (dwim-tab-localize-context 'template/expand)
  (dwim-tab-localize-context 'template/next)

  (local-set-key (kbd "C-c t l") 'templates/list)

  (local-set-key (kbd "C-c t i") 'template/insert)

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

(defun template/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
    (let (tmpsource cands result rmap)
      (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
      (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
      (setq tmpsource
        (list
          (cons 'name prompt)
          (cons 'candidates cands)
          '(action . (("Expand" . (lambda (selection) selection))))
          ))
      (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
      (if (null result)
        (signal 'quit "user quit!")
        (cdr (assoc result rmap))))
    nil))

(defun template/insert ()
  (interactive)
  (yas-insert-snippet))

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
