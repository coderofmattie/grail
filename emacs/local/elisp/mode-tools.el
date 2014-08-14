

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

(defun strip-minor-mode-keymap ( target-keymap )
  (let
    ((stripped-list '()))

    (mapc
      (lambda (minor-keymap)
        (unless (eq target-keymap (car minor-keymap))
          (setq stripped-list (cons minor-keymap stripped-list)) ))
      minor-mode-map-alist)

    (setq minor-mode-map-alist stripped-list) ))

;; remap the mode associated with a file

(defun remap-assoc-mode-to ( old-mode new-mode )
  ;; it looks weird because it uses setcdr side effects
  (mapc
    (lambda (pair)
      (if (eq (cdr pair) old-mode)
        (setcdr pair new-mode)))
    (append auto-mode-alist interpreter-mode-alist)) )

(provide 'mode-tools)
