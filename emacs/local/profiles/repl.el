;;----------------------------------------------------------------------
;; repl capabilities.
;;
;; a uniform way of creating repl's
;;----------------------------------------------------------------------
(defvar repl-languages nil)

(defvar-local repl-buffer-name nil)
(defvar-local repl-ring-name nil)
(defvar-local repl-create-fn nil)

(defun repl-add-language ( language start-fn )
  (setq repl-languages (cons (cons language start-fn) repl-languages)))

(defun repl-create ()
  (funcall repl-create-fn repl-buffer-name)

  (with-current-buffer repl-buffer-name
    (buffer-ring-add repl-ring-name)) )

(defun repl-invoke ()
  (unless (get-buffer repl-buffer-name)
    (repl-create))

  (pop-to-buffer (get-buffer repl-buffer-name) nil t)
  (switch-to-buffer repl-buffer-name))

(defun repl-setup-for-langauge ( buffer-name ring-name create-fn )
  (setq
    repl-buffer-name buffer-name
    repl-ring-name ring-name
    repl-create-fn create-fn)

  (configure-for-repl 'repl-invoke))

(defun repl-setup-for-command ( language buffer-name ring-name create-fn )
  (lexical-let
    ((lexical-buffer-name buffer-name)
     (lexical-ring-name ring-name)
     (lexical-create-fn create-fn))

    (repl-add-language language
      (lambda ()
        (let
          ((repl-buffer-name lexical-buffer-name)
           (repl-ring-name lexical-ring-name)
           (repl-create-fn lexical-create-fn))

          (repl-invoke) )) )))

(defun repl ( lang )
  "start a repl for the language specified"
  (interactive "MLanguage: ")
  (catch 'abort
    (let
      ((found-lang (assoc lang repl-languages)))

      (unless found-lang
        (message "could not find repl for language %s" lang)
        (throw 'abort t))

      (funcall (cdr found-lang)) )))
