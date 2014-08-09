;;----------------------------------------------------------------------
;; configure the display
;;----------------------------------------------------------------------

;; misc graphical settings
(setq-default
  use-dialog-box nil
  cursor-type 'hollow)

(blink-cursor-mode 0)

;; every fucking measure possible seems to be required
;; to change the color of the fucking cursor.

;; set some defaults for frames.
(grail-merge-frame-parameters 'default-frame-alist
  '(underline . nil)
  '(inverse-video . nil)
  '(box . nil)
  '(strike-through . nil)
  '(overline . nil)
  '(mouse-color . "red")
  '(cursor-color . "yellow") )

;; set some defaults for frames.
(grail-merge-frame-parameters 'initial-frame-alist
  '(underline . nil)
  '(inverse-video . nil)
  '(box . nil)
  '(strike-through . nil)
  '(overline . nil)
  '(mouse-color . "red")
  '(cursor-color . "yellow") )




