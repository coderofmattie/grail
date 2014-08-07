;;----------------------------------------------------------------------
;; elisp.el
;;----------------------------------------------------------------------
(require 'async-exec)
(require 'cm-string)

;; make sure that the pretty printer doesn't truncate which frustrates my
;; development.

(setq
 print-length nil
 eval-expression-print-level nil
 print-level nil)

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

;;
;; generic close proc buffer
;;

(defun proc-close-on-exit/window ( &optional proc-buffer )
  (async-exec-sentinel
    (or proc-buffer (current-buffer))
    (lambda ( proc-buffer )
      ;; if proc is dead touching the buffer except to kill is a error. trap
      ;; those situations and just kill it anyways.
      (condition-case nil
        ;; this is broken for reasons unknown anyways. FUCK I hate term mode.
        (with-current-buffer proc-buffer
          (other-window 1)
          (delete-other-windows proc-buffer))
        (error nil))

      (kill-buffer proc-buffer) )) )
