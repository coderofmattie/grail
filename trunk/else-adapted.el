;;----------------------------------------------------------------------
;;
;;----------------------------------------------------------------------
(require 'else-mode)

(setq else-mode-template-dir (concat (getenv "HOME") "/system/emacs/else/"))

(defun else-language-spec-p ( lang )
  "determine if a language definition has been loaded for lang"
  (if (assoc lang else-Language-Definitions)
    t
    nil)
  )

;; (else-language-spec-p "perl5")  - should be false
;; (else-language-spec-p "Empty")  - shoule be true

(defun else-reload-minimal ( &optional language-name )
  "reload the minimal definition of the else-mode language clearing all defined token expansions."

  (interactive)
  (let*
    ((lang (or language-name source-language))
     (template-path (concat else-mode-template-dir lang ".lse")))

    (if (file-readable-p template-path)
      (save-excursion
        (with-temp-buffer
          (progn
            (beginning-of-buffer)
            (insert-file-contents-literally template-path nil nil nil t )
            (else-compile-buffer)
            )))
      )
  ))

(defun minimal-else-language-def ( language-name )
  ;; create an alternative loading scheme. Instead of a language defining a complete
  ;; or base set of tokens , load only the language settings.

  ;; try to establish a minimal else language definition for the value of language-name.
  (or
    ;; already loaded ?
    (else-language-spec-p language-name)

    ;; attempt load from the else directory.
    (else-reload-minimal language-name)
    ))

;;----------------------------------------------------------------------
;; the beginning of the else functions moving to here.
;;----------------------------------------------------------------------

(defun else-load-xml ( template )
  "load a XML else template."

  (interactive "f else XML file ?")

  (save-excursion
    (let
      ((language source-language)
        (path (expand-file-name template)))

      (with-temp-buffer
        (if (and
              (file-readable-p path)

              (= 0 (call-process
                     (concat else-mode-template-dir "/assemble")     ;; translater program
                     nil                                             ;; stdin is /dev/null
                     (list (current-buffer) nil)                     ;; discard stderr , stdout -> current-buffer
                     nil                                             ;; don't refresh
                     language path))                                 ;; arguements are language and input file.
              (progn
                (beginning-of-buffer)
                (else-compile-buffer)))
          (message "Loaded else XML file: %s" template)
          (message "Failed to load else XML file: %s" path)
          ))))
  )

