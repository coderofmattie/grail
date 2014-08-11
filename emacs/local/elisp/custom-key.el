;;----------------------------------------------------------------------
;; custom-key
;;----------------------------------------------------------------------
(require 'subr-x)

(defun keybindings-help-first-line ( fn )
  (let
    ((docs (documentation fn)))

    (if docs
      (car
        (split-string docs "
"))
      "?")))

(defun keybindings-local-display ( group-name keymap )
  (format "key set: %s
%s"
    group-name
    (string-join
      (mapcar
        (lambda ( keymap-pair )
          (format "(%s) %s"
            (char-to-string (car keymap-pair))
            (keybindings-help-first-line (cdr keymap-pair))) )
        (cdr keymap))
      "
") ))

(defun keybindings-global-display ( keymap )
  (string-join
    (mapcar
      (lambda ( keymap-pair )
        (format "(%s) %s"
          (char-to-string (car keymap-pair))
          (keybindings-help-first-line (cdr keymap-pair))) )
      (cdr keymap))
    "
") )

(defconst keybindings-help-buffer-name "*keybindings help*")

(defun keybindings-help-quit ()
  (interactive)

  (other-window 1)
  (delete-other-windows)

  (kill-buffer (get-buffer keybindings-help-buffer-name)) )

(defun keybindings-help-local ( group-name keymap )
  (lexical-let
    ((doc-string (keybindings-local-display group-name keymap)))

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

(defun keybindings-help-global ()
  (interactive)

  (switch-to-buffer
    (with-current-buffer
      (pop-to-buffer
        (get-buffer-create keybindings-help-buffer-name))

      (erase-buffer)
      (insert "Custom Key Groups\n")

      (mapc
        (lambda (x)

          (insert (format "keys: C-c <%s> %s\n%s\n\n"
                    (elt x 0)
                    (elt x 1)
                    (keybindings-global-display (elt x 2)) )) )
        custom-keys-descriptions )

      (local-set-key (kbd "q") 'keybindings-help-quit)
      (message "press \"q\" to quit help.")

      (current-buffer) )) )

(defun custom-key-group-new (chord description keymap)
  (vector chord description keymap) )

(defun custom-key-group-register ( chord description key-map)
  (setq custom-keys-descriptions
    (cons (custom-key-group-new chord description key-map) custom-keys-descriptions)) )

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

     (define-key key-map "h" (keybindings-help-local ,description key-map))

     (,(if global
         'global-set-key
         'local-set-key)
       (kbd (concat "C-c " ,chord)) key-map)

     (custom-key-group-register ,chord ,description key-map) ))

(global-set-key (kbd "C-c h h") 'keybindings-help-global)

(provide 'custom-key)
