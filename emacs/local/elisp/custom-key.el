;;----------------------------------------------------------------------
;; custom-key
;;----------------------------------------------------------------------

(defun keybindings-help-first-line ( fn )
  (let
    ((docs (documentation fn)))

    (if docs
      (car
        (split-string docs "
"))
      "?")))

(defun keybindings-help-display ( group-name keymap )
  (format "key set: %s
%s"
    group-name
    (cm-string-join "
"
      (mapcar
        (lambda ( keymap-pair )
          (format "(%s) %s"
            (char-to-string (car keymap-pair))
            (keybindings-help-first-line (cdr keymap-pair))) )
        (cdr keymap)) ) ))

(defconst keybindings-help-buffer-name "*keybindings help*")

(defun keybindings-help-quit ()
  (interactive)

  (other-window 1)
  (delete-other-windows)

  (kill-buffer (get-buffer keybindings-help-buffer-name)) )

(defun keybindings-help-fn ( group-name keymap )
  (lexical-let
    ((doc-string (keybindings-help-display group-name keymap)))

    (lambda ()
      (interactive)

      (switch-to-buffer
        (with-current-buffer
          (pop-to-buffer
            (get-buffer-create keybindings-help-buffer-name))

          (insert doc-string)

          (local-set-key (kbd "q") 'keybindings-help-quit)
          (message "press \"q\" to quit help.")

          (current-buffer))) )) )

(defvar custom-keys-descriptions '()
  "descriptions of custom key groups")

(defmacro custom-key-group ( description chord global &rest body )
  `(let
     ((key-map  (make-sparse-keymap)))

     ,@(mapcar
         (lambda ( key-fn-pair )
           `(define-key key-map
              ,(car key-fn-pair)

              ,(if (symbol-function (cdr key-fn-pair))
                 `',(cdr key-fn-pair)
                 (cdr key-fn-pair)) ) )
         body)

     (define-key key-map "h" (keybindings-help-fn ,description key-map))

     (,(if global
         'global-set-key
         'local-set-key)
       (kbd (concat "C-c " ,chord)) key-map)

     (setq custom-keys-descriptions (cons (cons ,chord ,description) custom-keys-descriptions)) ))

(provide 'custom-key)
