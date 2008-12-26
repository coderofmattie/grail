;;----------------------------------------------------------------------
;; xml-code.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

(require 'cm-list)

;;----------------------------------------------------------------------
;; global variable declarations
;;----------------------------------------------------------------------

;; paths

(defvar xml-code-dir (concat grail-elisp-root "xml-code/")
  "the root directory of xml-code which contains tools, xml documents, and generated templates")

(defvar xml-code-repository-dir (concat xml-code-dir "repository/")
  "the repository of template documents")

(defvar xml-code-templates-dir (concat xml-code-dir "templates/")
  "the generated templates directory")

;; flags

(defvar xml-code-components-loaded nil
  "An internal flag indicating whether the underlying template system successfully loaded.")

;; template system indirection layer

(defvar xml-code-template-expand nil
  "An internal pointer to the template expand entry point.")

;; tables

(defvar xml-code-languages nil
  "An internal list of the languages with templates available")

;;----------------------------------------------------------------------
;; local variable declarations
;;----------------------------------------------------------------------

(defvar xml-code-language nil
  "An internal buffer local variable specifying The current programming language as an index into
   available templates.")

;;----------------------------------------------------------------------
;; load the template system
;;----------------------------------------------------------------------

;; yasnippet is a superb template system
(robust-load-elisp "yasnippet"
  (require 'yasnippet)

  (setq
    yas/dont-activate t                   ;; don't activate automatically
    yas/fallback-behavior 'return-nil     ;; fallback is return nil so that dwim-tab handles fallback.

    yas/trigger-key       (kbd "C-l e"))  ;; a manual trigger key.

  ;; configure xml-code for yasnippet
  (setq
    xml-code-template-expand 'yas/expand

    xml-code-components-loaded t))

(defun xml-code-contextual-expand ()
  "xml-code-contextual-expand

   When not in a comment attempt template expansion. nil is returned if it is out of context
   or the template expansion fails.
  "
  (if (not (eq 'font-lock-comment-face (face-at-point)))
    (funcall xml-code-template-expand)
    nil))

(defun xml-code-for-language ( language )
  "xml-code-for-language LANGUAGE

   Initialize the buffer for language. return the Load Status for the language which is
   non-nil if templates are available.

   This function always initializes xml-code even if no templates were found.
  "
  (set (make-local-variable 'xml-code-language) language)

  (cdr (or
         (assoc language xml-code-languages)
         (setq xml-code-languages (cons language (xml-code-load-templates language))))))


(defun xml-code-templates-p ()
  "xml-code-templates-p

   return non-nil if xml-code and the buffer supports template expansion.
  "
  (and xml-code-components-loaded xml-code-language))


;;----------------------------------------------------------------------
;; stubs
;;----------------------------------------------------------------------

(defun xml-code-load-templates ( lang )
  "xml-code-load-templates LANGUAGE

   Attempt to load the templates for LANGUAGE. If the attempt is successful
   non-nil is returned.
  "
  nil)

;;----------------------------------------------------------------------
;; The really edgy feature I am looking for is the ability to generate
;; templates from a template.
;;
;; The canonical example is the for loop. I would ideally want to be
;; able to do something like: for (i = 0 ; i < n ; i++ )
;;
;; then in the body, I would want to be able to expand i to [i]. In fact
;; I would want to make the template for referencing i within that scope.
;;
;; the direction I am going in now involves leveraging something like
;; hippie-expand, where the lisp evaluation feature in yasnippet is used
;; to update the hippie expansion table.
;;
;; even better would be if the fields remained "linked" or mirrored.
;;
;; even the basic abbrev will take some work. it needs to be something
;; you can use on the fly. Add a region to a list sorted by alpha and length.
;; Hitting tab should keep expanding to the next size up, one by one.
;; naming them should be easy too.
;;
;; saving should be possible within comments.
;;
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; these parts are ugly. Not integrated correctly with exisitng code.
;;----------------------------------------------------------------------

;; (setq (concat (getenv "HOME") "/system/emacs/")
;; (setq xml-code-repository (concat (getenv "HOME") "/system/emacs/else/"))
;; (setq xml-code-tempaltes 
;; (setq else-mode-xml-alist '())

;; (defun else-language-spec-p ( lang )
;;   "determine if a language definition has been loaded for lang"
;;   (if (assoc lang else-Language-Definitions)
;;     t))

;; ;; (else-language-spec-p "perl5")  - should be false
;; ;; (else-language-spec-p "Empty")  - shoule be true

;; ;;----------------------------------------------------------------------
;; ;; IPC assembly
;; ;;----------------------------------------------------------------------

;; ;; else mode currently uses a external helper program "assemble" to
;; ;; translate the XML form to a representation loadable by else-mode/Emacs.

;; ;; the two stages of loading are assembly and actual loading. On a internal
;; ;; level the functions that perform translation through IPC are called
;; ;; else-xml-assemble-? , while the part that performs the actual loading
;; ;; are else-xml-load-?

;; (defun else-xml-load-dec ( buffer )
;;   "load a else macro definition into emacs, currently using the DEC template syntax"
;;   (progn
;;     (goto-char (point-min))
;;     (else-compile-buffer))
;;   )

;; (defun else-xml-assemble-files ( lang &rest file-list )
;;   "assemble and load the given files"
;;     (with-temp-buffer
;;       (and
;;         (= 0 (apply 'call-process
;;                (concat else-mode-xml-dir "/assemble")      ;; translator program
;;                nil                                         ;; no stdin
;;                (list (current-buffer) nil)                 ;; discard stderr , stdout -> current-buffer
;;                nil                                         ;; don't refresh
;;                lang file-list))                            ;; arguments are language and input files.

;;         (else-xml-load-dec (current-buffer))
;;         )))

;; (defun else-xml-assemble-region ( lang buffer start end )
;;   "assemble a given region"

;;   (with-temp-buffer
;;     (let ((load-buffer (current-buffer)))

;;       (and
;;         (= 0 (with-current-buffer buffer
;;                (apply 'call-process-region
;;                  start end                               ;; send the entire buffer to STDIN
;;                  (concat else-mode-xml-dir "/assemble")  ;; translater program.
;;                  nil                                     ;; don't delete the buffer contents
;;                  (list load-buffer nil)                  ;; discard stderr, stdout -> current-buffer
;;                  nil                                     ;; display is irrelevant.
;;                  lang (list "-"))))                      ;; program arguments.
;;         (else-xml-load-dec load-buffer)
;;       ))))

;; (defun else-xml-assemble-buffer ( lang buffer )
;;   "assemble an entire buffer"

;;   (with-current-buffer buffer
;;     (else-xml-assemble-region lang buffer (point-min) (point-max))))

;; ;; the user does not need to care about internal abstractions, so load
;; ;; at the user level means given the name of a document, construct a
;; ;; path, assemble the templates, and load the templates.

;; (defun else-xml-load-files ( language &rest file-list )
;;   "load a list of else xml files. Not a interactive function as
;;    it blindly loads the given list assuming that a wrapper has
;;    checked that the files actually exist."

;;   (if (apply 'else-xml-assemble-files language file-list)
;;     (message "else XML auto-load for: %s completed." language)
;;     (message "Failed else XML auto-load for: %s" language)
;;     ))

;; (defun else-xml-load ( template )
;;   "load else-XML templates from a document."

;;   (interactive "F else XML file ? ")

;;   (save-excursion
;;     (let
;;       ((path (or
;;                (file-if-readable (expand-file-name template))
;;                (file-if-readable (concat else-mode-xml-dir template ".xml"))
;;                ))
;;         )
;;       (if path
;;         (else-xml-assemble-files source-language path)
;;         (message "else-xml-load could not stat template %s" template))
;;       )))

;; ;;----------------------------------------------------------------------
;; ;; loader
;; ;;----------------------------------------------------------------------

;; ;; This is the beginning of an alternative loader for else. It is
;; ;; prototyped as a bolt-on to the existing mode. The high level user
;; ;; interface is essentially final as a "alist" keyed by language,
;; ;; where the value is a list of template files to load.

;; ;; The key difference from the original else mode is that a single
;; ;; file is no longer a complete definition of a language - where a
;; ;; file contains syntatic elements of the language and all of the
;; ;; templates for that language.

;; ;; An advantage of XML is that template defintions may be embedded in
;; ;; a several documents of arbitrary purpose (heterogenous). This
;; ;; requires a new loader where the alist is not implict ( filename
;; ;; inferred from mode name ), load-path is not used as a search
;; ;; mechanism, and multiple files may be given.

;; ;; The new mechanism uses else-mode-xml-alist to key a list of
;; ;; documents to a language name. The language name is no longer the
;; ;; mode name which is essential for handling multiple versions of a
;; ;; language gracefully, which can be redically different (perl5 vs
;; ;; perl6)

;; ;; TODO

;; ;; a far more advanced loader is described in the TODO, however it is
;; ;; merely a more flexible version of this concept That potentially is
;; ;; designed enough to integrate into the original mode.

;; (defun else-xml-load-language ( language-name )
;;   ;; only attempt to load a minimal language definition if there is no
;;   ;; language definition.

;;   (or
;;     ;; already loaded ?
;;     (else-language-spec-p language-name)

;;     ;; attempt load from the else directory.
;;     (else-xml-reset-language language-name)
;;     ))

;; (defun else-xml-reset-language ( &optional language-name )
;;   "reset the language by clearing all templates and re-loading the minimal language defintion."

;;   ;; major unchecked assumption that redefining a language clears all the templates
;;   (interactive)
;;   (let*
;;     ((lang (or language-name source-language))
;;      (template-path (concat else-mode-xml-dir lang ".lse")))

;;     (if (file-readable-p template-path)
;;       (save-excursion
;;         (with-temp-buffer
;;           (goto-char (point-min))
;;           (insert-file-contents-literally template-path nil nil nil t)
;;           (else-compile-buffer)
;;           ))
;;       )
;;   ))

;; (defun else-xml-load-language-alist ( lang )
;;   "load all of the xml files listed in else-mode-xml-alist for: language, the sole parameter"
;;   (lexical-let
;;     ((file-list (else-xml-alist-expand lang)))
;;     (if file-list
;;       (apply 'else-xml-assemble-files lang file-list)
;;       (message "there are no xml files listed in the else-mode-xml-alist for language %s", lang))
;;     ))

;; (defun else-xml-alist-expand ( lang )
;;   "expand the list of else XML files."
;;   ;; a little nutty ? but it works.

;;     ;; map-filter-nil the full XML paths with a file exists test.
;;     (apply 'map-filter-nil 'file-if-readable

;;       ;; concat the xml-dir path with the file to form a path to the file.
;;       (mapcar (lambda ( xml-file )
;;                 ;; TODO: the concat is a good default for basname files, however if
;;                 ;; there is a directory part, or an absolute path it should not expand.
;;                 (concat else-mode-xml-dir xml-file))
;;         ;; fetch the list of xml files
;;         (cdr (assoc lang else-mode-xml-alist))))
;;   )

;; ;;----------------------------------------------------------------------
;; ;; create new templates
;; ;;----------------------------------------------------------------------

;; ;; These parts are true extensions of the exisintg codebase.

;; (defun else-xml-new ()
;;   "create a new macro definition"
;;   (interactive)

;;   ;; features
;;   ;; * pop a window in which to create a new macro.
;;   ;; * enable the "compilation" of a template to try it out.
;;   ;; * simple way to then write the new macro to a existing file, or
;;   ;;   create one.

;;   ;; need to make a window pop a new buffer ala diff. need to set it up
;;   ;; so that you can enter a macro, evaluate it for the current language
;;   ;; and then return to the code file.

;;   (lexical-let
;;     ((template-buffer (generate-new-buffer "else-XML macro"))
;;      (xml-major-mode (assoc-default "foo.xml" auto-mode-alist
;;                        'string-match))

;;       ;; the helpers need to be generated before we switch
;;       ;; to the template buffer so they can use the current-buffer
;;       ;; variables

;;       (to-file (else-xml-to-file))
;;       (to-asm  (else-xml-to-assembler))

;;       entry-point  ;; variable recording where the point should be positioned.
;;       )

;;     (with-current-buffer template-buffer
;;       (if xml-major-mode (funcall xml-major-mode))

;;       ;; when we kill the buffer get rid of the window associated so the user
;;       ;; doesn't have to tediously clean-up.
;;       (add-hook 'kill-buffer-hook 'rid-window t t)

;;       ;; set keys for saving the buffer, compiling the buffer, and
;;       ;; merging the buffer (TODO)

;;       (local-set-key (kbd "C-x C-s")
;;         ;; over-ride the usual "save-buffers" command with a custom one
;;         ;; that creates a valid xml document before writing to a file.
;;         ;; as a nice side-effect the current buffer is not associated
;;         ;; with a file. This is a plus since the current buffer is not
;;         ;; a valid document.

;;         ;; if you want to save the document fragment that is the current
;;         ;; buffer you can still use C-x-w.
;;         (lambda ()
;;           "write the tokens as valid xml to a file"
;;           (interactive)
;;           (else-xml-output-valid-xml (current-buffer) to-file)))

;;       (local-set-key (kbd "C-l c")
;;         (lambda ()
;;           "compile the buffer using the xml assembler"
;;           (interactive)
;;           (else-xml-output-valid-xml (current-buffer) to-asm)))

;;       (local-set-key (kbd "C-l m") 'else-xml-to-merge)

;;       ;; generate some minimal boilder-plate for ergonomics
;;       (insert "<token>\n  <name>")
;;       ;; the point doesn't really exist, so we use point-max which is end
;;       ;; of the buffer.
;;       (setq entry-point (point-max))
;;       (insert "</name>\n</token>\n")
;;       )

;;     (pop-to-buffer template-buffer)
;;     (goto-char entry-point)
;;     ))

;; (defun else-xml-output-valid-xml ( buffer output-method )
;;   "Finalize the tokens as a valid xml document and IPC to a processor."

;;   (with-temp-buffer

;;     ;; create a buffer with the token definition imbetween the xml declaration
;;     ;; and the document root.

;;     (insert "<?xml version=\"1.0\" encoding=\"US-ASCII\"?>\n")
;;     (insert "<else>")

;;     (lexical-let
;;       ((io-buffer (current-buffer)))

;;       (with-current-buffer buffer
;;         (append-to-buffer io-buffer (point-min) (point-max))))

;;     (insert "</else>")

;;     ;; output this buffer to the assembler
;;     (funcall output-method (current-buffer))
;;     ))

;; (defun else-xml-to-assembler ()
;;   "Generate a function that emits a given buffer to the assembler
;;    with the buffer's else-emit-target value as the language target."

;;   (lexical-let
;;     ((target source-language))

;;     (lambda ( buffer )
;;       (else-xml-assemble-buffer target buffer))
;;     ))

;; (defun else-xml-to-file ()
;;   "Generate a function that writes a given buffer to a file"

;;   (lambda ( buffer )
;;     (with-current-buffer buffer
;;       (let
;;         ;; set the default directory to the else-mode-dir
;;         ((default-directory else-mode-xml-dir))
;;         (write-file nil t)))
;;     ))

;; (defun else-xml-to-merge ( document )
;;   "merge the contents of a given buffer with a existing xml document by appending before
;;    the closing tag of the document root"
;;   (interactive "fXML Document? ")
;;   (lexical-let
;;     ((merge-from (current-buffer)))
;;     (if (and (find-file document) (xml-before-doc-close))
;;       (progn
;;         (open-line 2)
;;         (insert-buffer-substring merge-from)
;;         ))))

;; (defun else-xml ()
;;   "try to setup else-xml, return t on success nil on failure."
;;   (catch 'dont-install

;;     (unless (and (boundp 'source-language) (stringp source-language))
;;       (throw 'dont-install nil))

;;     (unless (else-xml-load-language source-language)
;;       (throw 'dont-install nil))

;;     ;; localize the current language to the buffer and set it properly
;;     (else-establish-language source-language)

;;     (else-mode)

;;     (else-xml-load-language-alist source-language)

;;     t))

(provide 'xml-code)
