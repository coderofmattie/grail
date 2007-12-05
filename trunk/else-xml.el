;;----------------------------------------------------------------------
;; else-xml.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

(setq else-mode-xml-dir (concat (getenv "HOME") "/system/emacs/else/"))
(setq else-mode-xml-alist '())

(defun else-language-spec-p ( lang )
  "determine if a language definition has been loaded for lang"
  (if (assoc lang else-Language-Definitions)
    t))

;; (else-language-spec-p "perl5")  - should be false
;; (else-language-spec-p "Empty")  - shoule be true

(defun else-reload-minimal ( &optional language-name )
  "reload the minimal definition of the else-mode language clearing all defined token expansions."

  ;; major unchecked assumption that redefining a language clears all the templates
  (interactive)
  (let*
    ((lang (or language-name source-language))
     (template-path (concat else-mode-xml-dir lang ".lse")))

    (if (file-readable-p template-path)
      (save-excursion
        (with-temp-buffer
          (beginning-of-buffer)
          (insert-file-contents-literally template-path nil nil nil t )
          (else-compile-buffer)
          ))
      )
  ))

(defun else-xml-file-if-exists ( file )
  "this function was created because file-readable-p is brain-damaged in that it returns t
   instead of the path it was given which neccesitates this silly wrapper. Consider sending
   this upstream as a patch to file-readable-p"

  (if (file-readable-p file)
    file))

;;----------------------------------------------------------------------
;; These functions are purely for the XML add-on functionality to else.
;;----------------------------------------------------------------------

(defun else-xml-new ()
  "create a new macro definition"
  (interactive)

  ;; features
  ;; * pop a window in which to create a new macro.
  ;; * enable the "compilation" of a template to try it out.
  ;; * simple way to then write the new macro to a existing file, or
  ;;   create one.

  ;; need to make a window pop a new buffer ala diff. need to set it up
  ;; so that you can enter a macro, evaluate it for the current language
  ;; and then return to the code file.

  (let
    ((language source-language)
     (template-buffer (generate-new-buffer "else-XML macro"))
     (xml-major-mode (assoc-default "foo.xml" auto-mode-alist
                       'string-match))
      )

    (with-current-buffer template-buffer
      ;; set a emit target for compiling the template so we
      ;; can compile the template without prompting the user.

      (set (make-local-variable 'else-emit-target) language)

      (if xml-major-mode (funcall xml-major-mode))

      ;; when we kill the buffer get rid of the window associated so the user
      ;; doesn't have to tediously clean-up.
      (add-hook 'kill-buffer-hook 'rid-window t t)
      )

    (pop-to-buffer template-buffer)
    ))

(defun else-xml-load-language ( language-name )
  ;; create an alternative loading scheme. Instead of a language defining a complete
  ;; or base set of tokens , load only the language settings.

  ;; try to establish a minimal else language definition for the value of language-name.
  (or
    ;; already loaded ?
    (else-language-spec-p language-name)

    ;; attempt load from the else directory.
    (else-reload-minimal language-name)
    ))

(defun else-xml-alist-expand ( lang )
  "expand the list of else XML files."
  ;; a little nutty ? but it works.

    ;; map-reduce the full XML paths with a file exists test.
    (apply 'map-reduce 'else-xml-file-if-exists

      ;; concat the xml-dir path with the file to form a path to the file.
      (mapcar (lambda ( xml-file )
                (concat else-mode-xml-dir xml-file))
        ;; fetch the list of xml files
        (cdr (assoc lang else-mode-xml-alist))))
  )

(defun else-xml-load-language-alist ( lang )
  "load all of the xml files listed in else-mode-xml-alist for: language, the sole parameter"
  (let
    ((file-list (else-xml-alist-expand lang)))
    (if file-list
      (apply 'else-xml-load-files lang file-list)
      (message "there are no xml files listed in the else-mode-xml-alist for language %s", lang))
    ))

(defun else-xml-load ( template )
  "load a XML else template."

  (interactive "F else XML file ? ")

  (save-excursion
    (let
      ((path (or
               (else-xml-file-if-exists (expand-file-name template))
               (else-xml-file-if-exists (concat else-mode-xml-dir template ".xml"))
               ))
        )
      (if path
        (else-xml-load-files source-language path)
        (message "else-xml-load could not stat template %s" template))
      )))

(defun else-xml-load-files ( language &rest file-list )
  "load a list of else xml files. Not a interactive function as it blindly loads the given list assuming
   that a wrapper has checked that the files actually exist."

  (with-temp-buffer
    (if (and
          (not (null file-list))                                 ;; abort on a empty file list

          (= 0 (apply 'call-process
                 (concat else-mode-xml-dir "/assemble")          ;; translater program
                 nil                                             ;; stdin is /dev/null
                 (list (current-buffer) nil)                     ;; discard stderr , stdout -> current-buffer
                 nil                                             ;; don't refresh
                 language file-list))                            ;; arguements are language and input file.
          (progn
            (beginning-of-buffer)
            (else-compile-buffer))
          )
      (message "else XML auto-load for: %s completed." language)
      (message "Failed else XML auto-load for: %s" language)
      ))
  )
