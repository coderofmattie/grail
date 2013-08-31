;;----------------------------------------------------------------------
;; template profile.
;;
;; support for the yasnippet template system with a generic interface
;; layered on. This glue API prevents the yasnippet API from creeping
;; into the rest of my config.
;;----------------------------------------------------------------------

(defvar templates-enabled nil
  "a boolean for when the template system has been loaded.")

(defun templates-enabled-p ()
  "templates-enabled-p

   return non-nil when templates are enabled.
  "
  (when templates-enabled t))

;;----------------------------------------------------------------------
;; load yassnippet template system
;;----------------------------------------------------------------------

;; (grail-load 'yasnippet (grail-define-installer
;;                          "yasnippet" "svn"
;;                          "http://yasnippet.googlecode.com/svn/trunk/"))

(grail-load 'yasnippet (grail-define-installer "yasnippet"
                         "pkg"
                         'yasnippet))

;;----------------------------------------------------------------------
;; configure yassnippet
;;----------------------------------------------------------------------

;; don't automatically activate.
(setq yas/dont-activate t)

;; This is a really ugly situation. The default non-nil behavior is to
;; unbind the yasnippet keymap and call the previous keymap function
;; which is of course dwim-tab. When calling yas/expand from inside
;; dwim-tab you get an infinite loop. setting this to nil fixes this.
(setq yas/fallback-behavior nil)

;; ignore filenames as triggers since the trigger name can be set in
;; the snippet without the restrictions imposed by the filesystem on
;; naming
(setq yas/ignore-filenames-as-triggers t)

;; yasnippet-bundle does not define this ? WTF!!
(if (intern-soft "yas/snippet-dirs")
  (defvar yas/snippet-dirs '()) )

;;----------------------------------------------------------------------
;; templates/* an interface for loading/creating templates.
;;----------------------------------------------------------------------

(defvar template-languages (make-hash-table)
  "a list of the languages with templates loaded")

(defconst templates-root
  (expand-file-name (concat grail-elisp-root "templates/yasnippet/"))
  "the yasnippet tree path relative to grail-elisp-root")

(defun templates/configure-mode ( language )
  "templates/configure-mode

    Configure a buffer for templates by setting the template langauge,
    activating the template minor mode, and setting up keybindings.
  "
  ;; set a buffer local variable describing which template language to use
  ;; for the buffer.
  (set (make-local-variable 'template-language) language)

  ;; activate yasnippet in the buffer
  (yas/minor-mode)

  ;; setup key-bindings for templates.
  (local-set-key (kbd "C-c t r") 'templates/reload)
  (local-set-key (kbd "C-c t l") 'templates/list)

  (local-set-key (kbd "C-c t e") 'template/expand)
  (local-set-key (kbd "C-c t n") 'template/next)

  (local-set-key (kbd "C-c t c") 'template/new)
  )

(defun templates/configure-language ( language &rest modes )
  "templates/configure-language LANGUAGE &REST MODES

   Setup templates for a language globaly. LANGUAGE is a
   string with the language name which should be a path
   relative to TEMPLATES-ROOT. the modes is a series of
   strings such as \"emacs-lisp-mode\". A function to
   configure the mode/buffer will be added to the
   entry point hook for the mode.
  "
  (let
    ((templates-path (concat templates-root language)))

    (unless (gethash language template-languages)
      (puthash language templates-path template-languages)

      ;; add it to the list of paths kept by yasnippet
      (if yas/snippet-dirs
        (setq yas/snippet-dirs (cons templates-path yas/snippet-dirs))
        (setq yas/snippet-dirs (list templates-path)))

      ;; load the templates for the first time.
      (templates/load-language language)

      (mapc
        (lambda ( mode )
          (let
            ((mode-hook (intern (concat mode "-hook"))))

            ;; add a hook function that initializes a mode for a given template language
            (lexical-let
              ((mode-language language))
              (add-hook mode-hook (lambda ()
                                    (templates/configure-mode mode-language))))))
        modes))
    ))

(defun templates/load-language ( language )
  "templates/load-language LANGUAGE

   Load the templates for LANGUAGE.
  "
  (let
    ((path (gethash language template-languages)))

    (if path
      (progn
        (yas/load-directory path)
        (message "loading templates from %s for language %s" path language))
      (message "unable to locate templates for language %s " language) )))

(defun templates/reload ()
  "templates/reload

   Reload the templates for the current language.
  "
  (interactive)
  (templates/load-language template-language))

(defun templates/list ()
  "templates/list

   List the current template table for the buffer's mode.
  "
  (interactive)
  (yas/describe-tables))

;;----------------------------------------------------------------------
;; template commands.
;;----------------------------------------------------------------------

(defun template/in-field-p ()
  "template/in-field-p

   return true when the point is in a yasnippet field
  "
  (when (mode-overlay-at-point-p 'yas/snippet-reference) t))

(defun template/expand ()
  "template/expand

   assuming the point is at a template name expand the template.
  "
  (interactive)
  (when (equal 'expanded (yas/expand)) (throw 'terminate-complete t))
  nil)

(defun template/next ()
  "template/next

   move to the next field in the template.
  "
  (interactive)
  (when (template/in-field-p)
    (yas/next-field-group)
    t))

(defun template/new ()
  "templates/new

   create a new template.
  "
  (interactive)
  (yas/new-snippet))

;;----------------------------------------------------------------------
;; now that the API is complete mark that the template system is enabled.
;;----------------------------------------------------------------------

;; signal that a template system is activated
(setq templates-enabled t)

;;----------------------------------------------------------------------
;; configure the various languages for template support.
;;----------------------------------------------------------------------

;; always load the lisp templates.
(templates/configure-language "lisp/elisp" "elisp-interaction-mode" "emacs-lisp-mode")

(eval-after-load 'cperl-mode
  '(templates/configure-language "perl" "cperl-mode"))

(eval-after-load 'nxml-mode
  '(templates/configure-language "web" "nxml-mode"))



