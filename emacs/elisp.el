;;----------------------------------------------------------------------
;; elisp.el
;;----------------------------------------------------------------------
(require 'cm-string)

;; increase the max eval depth to 4k. Hope this doesn't croak Emacs.
(setq max-lisp-eval-depth 4096)

;; make sure that the pretty printer doesn't truncate which frustrates my
;; development.

(setq
 print-length nil
 eval-expression-print-level nil
 print-level nil)

;; much like easy-mmode-define-keymap macro but with a little more
;; juice doing the defvar part as well.

(defmacro def-sparse-map ( symbol docstring &rest keys )
  "make it easy to define a keymap give the symbol, a docstring, followed by
   the usual (key 'symbol) lists."
  `(defvar ,symbol
     (let
       ((map (make-sparse-keymap)))
       ,@(mapcar (lambda (binding)
                   (list 'define-key 'map (car binding) (cadr binding))) keys)
       map)
     ,docstring))

(defun mode-overlay-at-pos ( pos mode-symbol )
  "determine if the point is in a flyspell overlay. given a overlay list
   which may be nil, translate via predicate into boolean values which
   are then evaluated by or."
  (interactive)
  (let
    ((overlay-list (overlays-at pos)))

    (when overlay-list
      (eval (cons 'or
              (mapcar
                (lambda ( overlay )
                  (if (overlay-get overlay mode-symbol) t)) overlay-list)))) ))

(defun mode-overlay-near-point-p ( mode-symbol )
  "determine if the point is in a flyspell overlay. given a overlay list
   which may be nil, translate via predicate into boolean values which
   are then evaluated by or."
  (interactive)
  (or (mode-overlay-at-pos (point) mode-symbol)
      (mode-overlay-at-pos (- (point) 1) mode-symbol)) )

(defun mode-overlay-at-point-p ( mode-symbol )
  "determine if the point is in a flyspell overlay. given a overlay list
   which may be nil, translate via predicate into boolean values which
   are then evaluated by or."
  (interactive)
  (mode-overlay-at-pos (point) mode-symbol))

(defun strip-list-last ( list )
  "strip the last element from a list"
  (if (consp (cdr list))
    (cons
      (car list)
      (strip-list-last (cdr list)))
    nil))

(defun run-hooks-with-arg ( hook-list &rest args )
  "run the hook list with arg"
  (mapc
    (lambda ( hook )
      (apply hook args))
      hook-list))

(defun todays-date ()
  "return today's date as a string."
  (let
    ((now (decode-time))
     (string nil))

    (mapc (lambda ( x )
            (setq string
              (if string
                (concat (number-to-string x) "-" string)
                (number-to-string x))))
      (list (nth 3 (decode-time)) (nth 4 (decode-time)) (nth 5 (decode-time)) ))
    string))

(defun strip-minor-mode-keymap ( target-keymap )
  (let
    ((stripped-list '()))

    (mapc
      (lambda (minor-keymap)
        (unless (eq target-keymap (car minor-keymap))
          (setq stripped-list (cons minor-keymap stripped-list)) ))
      minor-mode-map-alist)

    (setq minor-mode-map-alist stripped-list) ))

(defun buffer-modifiable-p (buffer)
  (with-current-buffer buffer
    (not (or (and (local-variable-p 'view-read-only) view-read-only)
             (and (local-variable-p 'buffer-read-only) buffer-read-only))) ))

(defun other-window-non-interactive ()
  (interactive)
  (other-window 1))

(defun select-word ()
  (interactive)
  (let
    ((begin nil)
     (end   nil)
     (pos   (point)))

    (catch 'fail
      (save-excursion
        (backward-word)
        (setq begin (point))

        (goto-char pos)

        (forward-word)
        (setq end (point))

        (unless (and (< begin pos )
                     (> end pos ))
          (throw 'fail nil)) )

      (push-mark begin)
      (goto-char end)
      t)))

(defun run-custom-hooks ( hook-list )
  (mapc
    (lambda ( hook )
      (funcall hook))
    hook-list))

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
    (string-join "
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
